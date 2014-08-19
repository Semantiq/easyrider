
function IO($scope) {
    var me = this;
    me.connect = function() {
        me.ws = new WebSocket("ws://" + window.location.host + "/api");
        me.ws.onmessage = function(e) {
            var msg = JSON.parse(e.data);
            $scope.$emit("newMessage", msg);
        };
        me.ws.onclose = function() {
            $scope.$apply(function() {
                $scope.role = null;
                $scope.connectionStatus = "Offline";
            });
            setTimeout(function() {
                me.connect();
            }, 100);
        };
        me.ws.onopen = function() {
            $scope.$apply(function() {
                $scope.connectionStatus = "Online";
                $scope.auditLog = [];
            });
        };
    };
    me.send = function(obj) {
        me.ws.send(JSON.stringify(obj));
    };
    me.connect();
}

var event_types = {
    "apps": function($scope, key, value) {
        if (value != "remove") {
            $scope.apps[key] = value;
        } else {
            delete $scope.apps[key];
        }
    },
    "stages": function($scope, key, value) {
        if (!$scope.stages[key.app_name]) {
            $scope.stages[key.app_name] = {};
        }
        if (value != "remove") {
            $scope.stages[key.app_name][key.stage_name] = value;
            return { notify: "info", message: key.app_name + "@" + key.stage_name, details: "Stage definition updated" };
        } else {
            delete $scope.stages[key.app_name][key.stage_name];            
        }
    },
    "instances": function($scope, key, value) {
        if (!$scope.instances[key.app_name]) {
            $scope.instances[key.app_name] = {};
            if (!$scope.instances[key.app_name][key.stage_name]) {
                $scope.instances[key.app_name][key.stage_name] = {};
            }
        }
        if (value != "remove") {
            $scope.instances[key.app_name][key.stage_name][key.id] = value;
        } else {
            delete $scope.instances[key.app_name][key.stage_name][key.id];
            return { notify: "info", message: "Instance " + key.id + " deleted", details: ""}
        }
    },
    "recommended_versions": function($scope, key, value) {
        if (!$scope.recommended_versions[key.app_name]) {
            $scope.recommended_versions[key.app_name] = {};
        }
        $scope.recommended_versions[key.app_name][key.stage_name] = value;
        return { notify: "info", message: "Version recommended for " + key.app_name + "@" + key.stage_name, details: value.version };
    },
    "instance_events": function($scope, key, value) {
        $scope.instance_events[key] = value;
        return { notify: "info", message: "Instance " + key, details: value.event + " " + value.version };
    }
};
function process_event($scope, toaster, message) {
    var handler = event_types[message.eventtype];
    if (handler) {
        if (message.type == "snapshot") {
            angular.forEach(message.data, function(item, index) {
                handler($scope, item.key, item.details);
            });
        } else {
            var result = handler($scope, message.key, message.);
            if (result && result.notify) {
                toaster.pop(result.notify, result.message, result.details);
            }
        }   
    } else {
        // TODO: log something
    }
}

var app = new angular.module("easyrider", ["toaster", "angularFileUpload"]);

app.directive('erVersions', function() {
    return {
        restrict: 'E',
        templateUrl: '/er-versions.html',
        scope: {
            versions: "=versions",
            selectedVersion: "=selectedVersion"
        },
        controller: function($scope) {
            $scope.select = function(version) {
                $scope.selectedVersion = version;
            };
        }
    };
});

app.controller("AppsCtrl", function($scope, toaster, $upload) {
    $scope.deployInstanceForm = {};
    $scope.addAppForm = {};
    $scope.addStageForm = {};

    $scope.apps = {};
    $scope.stages = {};
    $scope.instances = {};
    $scope.recommended_versions = {};
    $scope.instance_events = {};
    var io = new IO($scope);
    $scope.username = "test";
    $scope.password = "test";
    $scope.$on("newMessage", function(e, message) {
        $scope.$apply(function() {
            if (message.type == "welcome") {
                $scope.role = message.role;
                io.send({type: "subscribe", eventtypes: ["apps", "stages", "instances", "recommended_versions", "instance_events"]});
                io.send({type: "subscribe_versions", body: { limit: 10 }});
                $scope.activeTab = 'apps';
            } else if (message.event == "versions") {
                $scope.versions = {};
                angular.forEach(message.by_app, function(value, i) {
                    $scope.versions[value.app] = value.versions;
                });
            } else if (message.event == "new_version") {
                if ($scope.versions[message.version.app]) {
                    $scope.versions[message.version.app].unshift(message.version);
                } else {
                    $scope.versions[message.version.app] = [message.version];
                }
            } else if (message.event == "version_approved") {
                angular.forEach($scope.versions[message.version.app], function(version, i) {
                    if (version.number == message.version.number) {
                        version.approvals = message.version.approvals;
                    }
                });
            } else {
                process_event($scope, toaster, message);
            }
        });
    });
    $scope.login = function() {
        io.send({
            type: "login", 
            username: $scope.username,
            password: $scope.password
        });
    };
    $scope.startInstance = function(instanceId) {
        io.send({command: "tell_instance", body: {
            "id": instanceId,
            "message": "start"
        }});
    };
    $scope.stopInstance = function(instanceId) {
        io.send({command: "tell_instance", body: {
            "id": instanceId,
            "message": "stop"
        }});
    };
    $scope.deployInstance = function(instanceId, version) {
        io.send({command: "tell_instance", body: {
            "id": instanceId,
            "message": "deploy",
            "version": version
        }});
    };
    $scope.addApp = function(appName) {
        io.send({
            type: "setapp",
            app: {
                type: "app",
                name: appName,
                configuration: {
                    type: "configuration",
                    properties: null
                }
            }
        })
    };
    $scope.addStage = function(appName, stageName) {
        io.send({
            type: "setstage",
            stage: {
                type: "stage",
                app: appName,
                stage: stageName,
                configuration: null
            }
        });
    };
    $scope.uploadVersion = function(application, version, files) {
        $scope.uploadProgress = 0;
        var upload = $upload.upload({
            url: "/repo/upload",
            headers: {
                "Authorization": ("Basic " + btoa($scope.username + ":" + $scope.password))
            },
            data: {
                application: application,
                version: version
            },
            file: files[0],
            fileFormDataName: "content"
        });
        upload.progress(function(event) {
            $scope.uploadProgress = 100 * event.loaded / event.total;
        });
    };
});


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
    "apps": function($scope, key, value) {  $scope.apps[key] = value; },
    "stages": function($scope, key, value) {
        if (!$scope.stages[key.app_name]) {
            $scope.stages[key.app_name] = {};
        }
        $scope.stages[key.app_name][key.stage_name] = value;
    },
    "instances": function($scope, key, value) {
        if (!$scope.instances[key.app_name]) {
            $scope.instances[key.app_name] = {};
            if (!$scope.instances[key.app_name][key.stage_name]) {
                $scope.instances[key.app_name][key.stage_name] = {};
            }
        }
        $scope.instances[key.app_name][key.stage_name][key.id] = value;
    },
    "recommended_versions": function($scope, key, value) {
        if (!$scope.recommended_versions[key.app_name]) {
            $scope.recommended_versions[key.app_name] = {};
        }
        $scope.recommended_versions[key.app_name][key.stage_name] = value;
    },
    "instance_events": function($scope, key, value) {
        $scope.instance_events[key] = value;
    }
};
function process_event($scope, toaster, message) {
    var handler = event_types[message.event];
    if (handler) {
        if (message.snapshot) {
            angular.forEach(message.data, function(item, index) {
                handler($scope, item.key, item.value);
            });
        } else {
            handler($scope, message.key, message.value);
            toaster.pop("info", "Event " + message.event, "Just happened");
        }   
    } else {
        // TODO: log something
    }
}

var app = new angular.module("easyrider", ["toaster"]);

app.controller("AppsCtrl", function($scope, toaster) {
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
            if (message.event == "welcome") {
                $scope.role = message.role;
                $scope.username = message.username;
                io.send({command: "subscribe", body: ["apps", "stages", "instances", "recommended_versions", "instance_events"]});
                io.send({command: "subscribe_versions", body: { limit: 10 }});
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
        io.send({command: "login", body: {
            username: $scope.username,
            password: $scope.password
        }});
    };
    $scope.startInstance = function(instanceId) {
        io.send({command: "startInstance", body: {
            "instanceId": instanceId
        }});
    };
    $scope.stopInstance = function(instanceId) {
        io.send({command: "stopInstance", body: {
            "instanceId": instanceId
        }});
    };
});

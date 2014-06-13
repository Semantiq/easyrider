
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

var app = new angular.module("easyrider", []);

app.controller("AppsCtrl", ['$scope', function($scope) {
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
            } else if (message.event == "apps") {
                if (message.snapshot) {
                    $scope.apps = {};
                    angular.forEach(message.data, function(item, index) {
                        $scope.apps[item.key] = item.value;
                    });
                } else {
                    $scope.apps[message.key] = message.value;
                }
            } else if (message.event == "stages") {
                if (message.snapshot) {
                    $scope.stages = {};
                    angular.forEach(message.data, function(item, index) {
                        if (!$scope.stages[item.key.app_name]) {
                            $scope.stages[item.key.app_name] = {};
                        }
                        $scope.stages[item.key.app_name][item.key.stage_name] = item.value;
                    });
                } else {
                    // TODO: incremental stage updates
                }
            } else if (message.event == "instances") {
                if (message.snapshot) {
                    $scope.instances = {};
                    angular.forEach(message.data, function(item, index) {
                        if (!$scope.instances[item.key.app_name]) {
                            $scope.instances[item.key.app_name] = {};
                            if (!$scope.instances[item.key.app_name][item.key.stage_name]) {
                                $scope.instances[item.key.app_name][item.key.stage_name] = {};
                            }
                        }
                        $scope.instances[item.key.app_name][item.key.stage_name][item.key.id] = item.value;
                    });                                        
                } else {
                    // TODO: handle incremental updates
                }
            } else if (message.event == "recommended_versions") {
                if (message.snapshot) {
                    $scope.recommended_versions = {};
                    angular.forEach(message.data, function(item, index) {
                        if (!$scope.recommended_versions[item.key.app_name]) {
                            $scope.recommended_versions[item.key.app_name] = {};
                        }
                        $scope.recommended_versions[item.key.app_name][item.key.stage_name] = item.value;
                    });                    
                } else {
                    if (!$scope.recommended_versions[message.key.app_name]) {
                        $scope.recommended_versions[message.key.app_name] = {};
                    }
                    $scope.recommended_versions[message.key.app_name][message.key.stage_name] = message.value;
                }
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
                // TODO: log something
            }
        });
    });
    $scope.login = function() {
        io.send({command: "login", body: {
            username: $scope.username,
            password: $scope.password
        }});
    };
}]);

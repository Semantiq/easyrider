
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
                io.send({command: "subscribe_apps", body: {}});
                io.send({command: "subscribe_versions", body: { limit: 10 }});
            } else if (message.event == "apps") {
                $scope.apps = message.apps;
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

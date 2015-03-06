app.directive("sshNode", function() {
	return {
		restrict: "E",
		scope: {
			configuration: '=',
			state: '='
		},
		templateUrl: "/directives/sshNode/template.html",
		controller: ["$scope", 'ContainersConfiguration', 'SshNodes', 'Command', function($scope, SshNodes, Command) {
            $scope.getProperty = function(name) {
                for (var i = 0; i < $scope.configuration.properties.length; i++) {
                    var property = $scope.configuration.properties[i];
                    console.log("property.name == " + property.name);
                    if (property.name == name) {
                        return property.value;
                    }
                }
            };
		}]
	};
});

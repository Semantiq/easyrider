app.directive("sshNode", function() {
	return {
		restrict: "E",
		scope: {
			configuration: '=',
			state: '='
		},
		templateUrl: "/directives/sshNode/template.html",
		controller: ["$scope", 'ContainersConfiguration', 'SshNodes', 'Command', function($scope, SshNodes, Command) {
		}]
	};
});

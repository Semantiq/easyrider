app.directive("sshNode", function() {
	return {
		restrict: "E",
		scope: {
			configuration: '='
		},
		templateUrl: "/directives/sshNode/template.html",
		controller: ["$scope", 'ContainersConfiguration', 'SshNodes', 'Command', function($scope, SshNodes, Command) {
//			$scope.removeStage = function(id) {
//				Command.show(Stages.removeStageTemplate(id));
//			};
		}]
	};
});
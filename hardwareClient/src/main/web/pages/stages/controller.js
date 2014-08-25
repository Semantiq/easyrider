app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/stages", {
		templateUrl: "/pages/stages/template.html",
		controller: ["$scope", "Stages", "Command", "ContainersConfiguration",
		function($scope, Stages, Command, ContainersConfiguration) {
			$scope.Stages = Stages;
			$scope.ContainersConfiguration = ContainersConfiguration;

			$scope.addStage = function() {
				Command.show(Stages.addStageTemplate());
			};
			$scope.removeStage = function(id) {
				Command.show(Stages.removeStageTemplate(id));
			};
			$scope.addContainer = function(stageId) {
				Command.show(ContainersConfiguration.addContainerConfigurationTemplate(stageId));
			};
		}]
	});
}]);
app.directive("stage", function() {
	return {
		restrict: "A",
		scope: {
			stage: '='
		},
		templateUrl: "/directives/stage/template.html",
		controller: ["$scope", 'ContainersConfiguration', 'ContainersState', 'Stages', 'Command', function($scope, ContainersConfiguration, ContainersState, Stages, Command) {
			$scope.ContainersConfiguration = ContainersConfiguration;
			$scope.ContainersState = ContainersState;

			$scope.addStage = function() {
				Command.show(Stages.addStageTemplate());
			};
			$scope.removeStage = function(id) {
				Command.show(Stages.removeStageTemplate(id));
			};
			$scope.addContainer = function(stageId) {
				Command.show(ContainersConfiguration.addContainerConfigurationTemplate(stageId));
			};

			$scope.deployVersion = function(containerUpdatedEvent) {
				Command.show(ContainersConfiguration.deployVersionTemplate(containerUpdatedEvent.container.id));
			};
		}]
	};
});
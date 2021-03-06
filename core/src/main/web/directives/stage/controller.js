app.directive("stage", function() {
	return {
		restrict: "A",
		scope: {
			stage: '='
		},
		templateUrl: "/directives/stage/template.html",
		controller: ["$scope", 'ContainersConfiguration', 'ContainersState', 'DeployedVersions', 'Stages', 'Command', 'Orchestrator',
		        function($scope, ContainersConfiguration, ContainersState, DeployedVersions, Stages, Command, Orchestrator) {
			$scope.ContainersConfiguration = ContainersConfiguration;
			$scope.ContainersState = ContainersState;
			$scope.DeployedVersions = DeployedVersions;

			$scope.addStage = function() {
				Command.show(Stages.addStageTemplate());
			};
			$scope.removeStage = function(id) {
				Command.show(Stages.removeStageTemplate(id));
			};
			$scope.addContainer = function(stageId) {
				Command.show(ContainersConfiguration.addContainerConfigurationTemplate(stageId));
			};
            $scope.releaseVersionToStage = function(stageId) {
                Command.show(Orchestrator.releaseVersionTemplate(stageId))
            };
			$scope.deployVersion = function(container) {
				Command.show(ContainersConfiguration.deployVersionTemplate(container.id));
			};
			$scope.unDeployVersion = function(containerId, version) {
				Command.show(ContainersConfiguration.unDeployVersionTemplate(containerId, version));
			};

            $scope.startContainer = function(containerId, version) {
                Command.show(ContainersConfiguration.startContainerTemplate(containerId, version));
            };
            $scope.stopContainer = function(containerId) {
                Command.show(ContainersConfiguration.stopContainerTemplate(containerId));
            };
            $scope.removeContainer = function(containerId) {
                Command.show(ContainersConfiguration.removeContainerTemplate(containerId));
            };
            $scope.canStop = function(containerState) {
                return containerState.jsonClass == 'easyrider.Infrastructure$ContainerRunning';
            };
            $scope.canRemove = function(containerState) {
                return containerState.jsonClass == 'unknown' || containerState.jsonClass == 'easyrider.Infrastructure$ContainerCreated$';
            };
            $scope.saveStage = function(stage) {
                Command.show(Stages.updateStageTemplate(stage));
            };
            $scope.saveContainer = function(container) {
                Command.show(ContainersConfiguration.updateContainerConfigurationTemplate(container));
            };
		}]
	};
});

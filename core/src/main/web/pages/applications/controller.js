app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/applications", {
		templateUrl: "/pages/applications/template.html",
		controller: ["$scope", "Applications", "Command", "Stages", "ContainersConfiguration", "ContainersState", "Versions", "hotkeys", "$location", function($scope, Applications, Command, Stages, ContainersConfiguration, ContainersState, Versions, hotkeys, $location) {
		    $scope.ContainersState = ContainersState;
		    $scope.getApplications = function(any) {
		        return Applications.list;
		    };
		    $scope.getStages = function(application) {
                return Stages.stagesOfApplication(application.application.id);
		    };
		    $scope.getContainers = function(stage) {
		        return angular.forEach(ContainersConfiguration.containersInStage(stage.stage.id), function(container) {
                    container.status = function() {
                        return ContainersState.containerStatus(container.container.id);
                    };
		        });
		    };
		    $scope.showApplication = function(application) {
                $location.path("/application/" + application.application.id.id);
		    };
		    $scope.showStage = function(stage) {
                $location.path("/application/" + stage.stage.id.applicationId.id + "/stage/" + stage.stage.id.id);
		    };
			$scope.Applications = Applications;
			$scope.Stages = Stages;
			$scope.Versions = Versions;

			$scope.addApplication = function() {
				Command.show(Applications.addApplicationTemplate());
			};
			$scope.removeApplication = function(id) {
				Command.show(Applications.removeApplicationTemplate(id));
			};
			$scope.addStage = function(id) {
				Command.show(Stages.addStageTemplate(id));
			};
			$scope.versionsForApplication = function(appId, versions) {
			    var filtered = [];
			    for (var i in versions) {
			        var versionEvent = versions[i];
			        if (appId.id == versionEvent.eventDetails.eventKey.key[0])
			            filtered.push(versionEvent);
			    }
			    return filtered;
			};
		}]
	});
}]);
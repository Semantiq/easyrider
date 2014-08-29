app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/applications", {
		templateUrl: "/pages/applications/template.html",
		controller: ["$scope", "Applications", "Command", "Stages", "Versions", function($scope, Applications, Command, Stages, Versions) {
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
			    console.log(filtered);
			    return filtered;
			};
		}]
	});
}]);
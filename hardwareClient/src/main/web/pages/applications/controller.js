app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/applications", {
		templateUrl: "/pages/applications/template.html",
		controller: ["$scope", "Applications", "Command", "Stages", function($scope, Applications, Command, Stages) {
			$scope.Applications = Applications;
			$scope.Stages = Stages;

			$scope.addApplication = function() {
				Command.show(Applications.addApplicationTemplate());
			};
			$scope.removeApplication = function(id) {
				Command.show(Applications.removeApplicationTemplate(id));
			};
			$scope.addStage = function(id) {
				Command.show(Stages.addStageTemplate(id));
			};
		}]
	});
}]);
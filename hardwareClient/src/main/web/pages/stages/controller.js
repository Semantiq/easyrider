app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/stages", {
		templateUrl: "/pages/stages/template.html",
		controller: ["$scope", "Stages", "Command", function($scope, Stages, Command) {
			$scope.Stages = Stages;
			$scope.addStage = function() {
				Command.show(Stages.addStageTemplate());
			};
		}]
	});
}]);
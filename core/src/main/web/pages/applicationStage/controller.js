app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/application/:appId/stage/:stageId", {
		templateUrl: "/pages/applicationStage/template.html",
		controller: ["$scope", "Stages", "$routeParams", function($scope, Stages, $routeParams) {
			$scope.appId = $routeParams.appId;
			$scope.stageId = $routeParams.stageId;

			$scope.stage = function() {
			    if (Stages.stages.snapshot) {
				    return Stages.stages.snapshot.entries[$routeParams.appId + ":" + $routeParams.stageId];
				}
			};
		}]
	});
}]);
app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/application/:appId/stage/:stageId", {
		templateUrl: "/pages/applicationStage/template.html",
		controller: ["$scope", "Stages", "$routeParams", function($scope, Stages, $routeParams) {
			$scope.appId = $routeParams.appId;
			$scope.stageId = $routeParams.stageId;

			$scope.stage = function() {
				for(var i in Stages.list) {
					var stageEvent = Stages.list[i];
					if(stageEvent.stage.id.id == $scope.stageId &&
						stageEvent.stage.id.applicationId.id == $scope.appId) {
						return stageEvent;
					}
				}
			};
		}]
	});
}]);
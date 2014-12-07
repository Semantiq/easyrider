app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/application/:appId", {
		templateUrl: "/pages/application/template.html",
		controller: ["$scope", "$routeParams", "Applications", function($scope, $routeParams, Applications) {
			$scope.appId = $routeParams.appId;
            $scope.application = function() {
                for (var i in Applications.list) {
                    var applicationEvent = Applications.list[i];
                    if (applicationEvent.application.id.id == $scope.appId) {
                        return applicationEvent;
                    }
                }
            };
		}]
	});
}]);
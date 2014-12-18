app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/application/:appId", {
		templateUrl: "/pages/application/template.html",
		controller: ["$scope", "$routeParams", "Applications", "Command", function($scope, $routeParams, Applications, Command) {
			$scope.appId = $routeParams.appId;
			Applications.onChange(function(list) {
                for (var i in list) {
                    var applicationEvent = Applications.list[i];
                    if (applicationEvent.application.id.id == $scope.appId) {
                        $scope.application = applicationEvent;
                    }
                }
			});

            $scope.saveApplication = function(application) {
                Command.show(Applications.updateApplicationTemplate(application));
            };
		}]
	});
}]);
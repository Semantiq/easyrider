app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/", {
		templateUrl: "/pages/index/template.html",
		controller: ["$scope", "Applications", function($scope, Applications) {
			$scope.Applications = Applications;
		}]
	});
}]);
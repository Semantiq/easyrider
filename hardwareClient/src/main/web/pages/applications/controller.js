app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/applications", {
		templateUrl: "/pages/applications/template.html",
		controller: ["$scope", "Applications", function($scope, Applications) {
			$scope.Applications = Applications;

			$scope.addApplication = function() {
				var application = new Applications.Application(prompt("Type application name"), []);
				Applications.addApplication(application);
			};
		}]
	});
}]);
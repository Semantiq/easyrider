app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/", {
		templateUrl: "/pages/index/template.html"
	});
}]);
var app = angular.module("easyrider", ["ngRoute"]);

app.config(["$locationProvider", function($locationProvider) {
	$locationProvider.html5Mode(true);
}]);
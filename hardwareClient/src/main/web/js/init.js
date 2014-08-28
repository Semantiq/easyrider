var app = angular.module("easyrider", ["ngRoute", "angularFileUpload"]);

app.config(["$locationProvider", function($locationProvider) {
	$locationProvider.html5Mode(true);
}]);
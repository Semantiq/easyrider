var app = angular.module("easyrider", ["easyriderSearch", "ngRoute", "angularFileUpload"]);

app.config(["$locationProvider", function($locationProvider) {
	$locationProvider.html5Mode(true);
}]);
var app = angular.module("easyrider", ["ngRoute", "angularFileUpload", "cfp.hotkeys"]);

app.config(["$locationProvider", function($locationProvider) {
	$locationProvider.html5Mode(true);
}]);

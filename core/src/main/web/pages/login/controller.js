app.controller("LoginController", ["$scope", "Api", function($scope, Api) {
	$scope.signIn = function() {
		Api.authenticate(new Api.objects.Authenticate());
	};
	$scope.Api = Api;
}]);
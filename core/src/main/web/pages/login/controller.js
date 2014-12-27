app.controller("LoginController", ["$scope", "Api", function($scope, Api) {
	$scope.signIn = function() {
		Api.authenticate(new Api.objects.Authenticate($scope.username, $scope.password));
	};
	$scope.Api = Api;
}]);
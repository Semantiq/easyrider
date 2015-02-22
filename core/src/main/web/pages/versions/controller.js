app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/versions/:applicationId", {
		templateUrl: "/pages/versions/template.html",
		controller: ["$scope", "$routeParams", "Command", "Versions", function($scope, $routeParams, Command, Versions) {
		    $scope.applicationId = $routeParams.applicationId;
            $scope.Versions = Versions;
            $scope.addLabel = function(version) {
                Command.show(Versions.addLabelTemplate(version));
            };
            $scope.deleteVersion = function(version) {
                Command.show(Versions.deleteVersionTemplate(version));
            };
		}]
	});
}]);
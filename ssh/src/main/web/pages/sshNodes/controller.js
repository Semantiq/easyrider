app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/sshNodes", {
		templateUrl: "/pages/sshNodes/template.html",
		controller: ["$scope", "Command", "SshNodes", function($scope, Command, SshNodes) {
		    $scope.SshNodes = SshNodes;
			$scope.addSshNode = function() {
				Command.show(SshNodes.addSshNodeTemplate());
			};
		}]
	});
}]);

app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/sshNodes", {
		templateUrl: "/pages/sshNodes/template.html",
		controller: ["$scope", "Command", "SshNodes", function($scope, Command, SshNodes) {
		    $scope.sshNodeConfigurationEvents = SshNodes.list;
			$scope.addSshNode = function() {
				Command.show(SshNodes.addSshNodeTemplate());
			};
//			$scope.removeApplication = function(id) {
//				Command.show(Applications.removeApplicationTemplate(id));
//			};
//			$scope.addStage = function(id) {
//				Command.show(Stages.addStageTemplate(id));
//			};
		}]
	});
}]);
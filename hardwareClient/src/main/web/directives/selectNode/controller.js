app.directive("selectNode", function() {
	return {
		restrict: "E",
		templateUrl: "/directives/selectNode/template.html",
		scope: {
			model: "="
		},
		controller: ["$scope", "SshNodes", function($scope, SshNodes) {
			$scope.SshNodes = SshNodes;
			$scope.ids = function() {
				var ids = [];
				for(var i in SshNodes.stateList)
					ids.push(SshNodes.stateList[i].nodeId);
				return ids;
			}
		}]
	};
});

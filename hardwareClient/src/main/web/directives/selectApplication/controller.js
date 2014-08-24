app.directive("selectApplication", function() {
	return {
		restrict: "A",
		templateUrl: "/directives/selectApplication/template.html",
		scope: {
			model: "="
		},
		controller: ["$scope", "Applications", function($scope, Applications) {
			$scope.Applications = Applications;
			$scope.ids = function() {
				var ids = [];
				for(var i in Applications.list)
					ids.push(Applications.list[i].application.id);
				return ids;
			}
		}]
	};
});
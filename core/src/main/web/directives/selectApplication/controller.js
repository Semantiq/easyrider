app.directive("selectApplication", function() {
	return {
		restrict: "A",
		templateUrl: "/directives/selectApplication/template.html",
		scope: {
			model: "="
		},
		controller: ["$scope", "Applications", function($scope, Applications) {
			$scope.Applications = Applications;
		}]
	};
});
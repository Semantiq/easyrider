app.directive("property", function() {
	return {
		restrict: "E",
		scope: {
			property: '='
		},
		templateUrl: "/directives/property/template.html",
		controller: ["$scope", function($scope) {
		}]
	};
});

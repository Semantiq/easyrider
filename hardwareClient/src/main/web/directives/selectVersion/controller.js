app.directive("selectVersion", function() {
	return {
		restrict: "E",
		templateUrl: "/directives/selectVersion/template.html",
		scope: {
			model: "="
		},
		controller: ["$scope", "Versions", function($scope, Versions) {
			$scope.ids = function() {
				var ids = [];
				for(var i in Versions.list)
					ids.push(Versions.list[i].version);
				return ids;
			}
		}]
	};
});
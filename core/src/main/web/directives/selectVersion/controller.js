app.directive("selectVersion", function() {
	return {
		restrict: "E",
		templateUrl: "/directives/selectVersion/template.html",
		scope: {
			model: "=",
			applicationId: "="
		},
		controller: ["$scope", "Versions", function($scope, Versions) {
		    $scope.Versions = Versions;
			$scope.versions = function(applicationId) {
			    var list = [];
			    angular.forEach($scope.Versions.versionsByApplicationId[applicationId], function(value) {
                    list.push(value.version);
			    });
			    return list;
			}
		}]
	};
});

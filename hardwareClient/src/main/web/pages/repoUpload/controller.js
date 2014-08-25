app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/repo/upload", {
		templateUrl: "/pages/repoUpload/template.html",
		controller: ["$scope", function($scope) {
		    $scope.upload = {
                applicationId: {}
		    };
		    $scope.start = function() {
                $scope.uploadProgress = 0;
//                var upload = $upload.upload({
//                    url: "/repo/upload",
//                    headers: {
//                        "Authorization": ("Basic " + btoa($scope.username + ":" + $scope.password))
//                    },
//                    data: {
//                        application: application,
//                        version: version
//                    },
//                    file: files[0],
//                    fileFormDataName: "content"
//                });
//                upload.progress(function(event) {
//                    $scope.uploadProgress = 100 * event.loaded / event.total;
//                });
		    };
		}]
	});
}]);
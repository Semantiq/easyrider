app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/repository/upload", {
		templateUrl: "/pages/repoUpload/template.html",
		controller: ["$scope", "$upload", "$location", function($scope, $upload, $location) {
		    $scope.upload = {
                applicationId: {}
		    };
		    $scope.uploading = false;
		    $scope.start = function() {
		        $scope.uploading = true;
                $scope.uploadProgress = 0;
                var upload = $upload.upload({
                    url: "/api/repository/upload",
                    headers: {
                        //"Authorization": ("Basic " + btoa($scope.username + ":" + $scope.password))
                    },
                    data: {
                        application: $scope.upload.applicationId.id,
                        version: $scope.upload.version
                    },
                    file: $scope.files[0],
                    fileFormDataName: "content"
                });
                upload.progress(function(event) {
                    $scope.uploadProgress = 100 * event.loaded / event.total;
                });
                upload.success(function(data) {
                   $location.path("/application/" + $scope.upload.applicationId.id + "/version/" + $scope.upload.version);
                });
		    };
		}]
	});
}]);
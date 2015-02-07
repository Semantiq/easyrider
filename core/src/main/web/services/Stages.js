app.service('Stages', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.stages = Api.subscribeSnapshot("easyrider.Applications$Stage", function(snapshot) {
	});

	me.addStageTemplate = function(applicationId) {
		return {
			jsonClass: "easyrider.Applications$CreateStage",
			stage: {
				id: {
					applicationId: applicationId || null,
					id: ""
				},
				properties: [ ]
			}
		};
	};
	me.removeStageTemplate = function(stageId) {
		return {
			jsonClass: "easyrider.Applications$RemoveStage",
			stageId: stageId
		};
	};
    me.updateStageTemplate = function(stage) {
        return {
            jsonClass: "easyrider.Applications$UpdateStage",
            stage: stage.stage
        };
    };

	me.stagesOfApplication = function(applicationId) {
		var stages = [];
        if (me.stages.snapshot) {
            angular.forEach(me.stages.snapshot.entries, function(value, key) {
                if(value.id.applicationId.id == applicationId.id) {
                    stages.push(value);
                }
            });
		}
		return stages;
	};

	Validators.addValidator("easyrider.Applications$CreateStage",
		function(command, reporter) {
		if(!command.stage.id.applicationId)
			reporter.fail("No application selected", "You must select application related to this stage");
		if(!Utils.isValidId(command.stage.id.id))
			reporter.fail("Invalid stage id", Utils.idInfo);
	});
}]);
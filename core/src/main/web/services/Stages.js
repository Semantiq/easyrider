app.service('Stages', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Applications$StageUpdatedEvent", []);
	me.list = me.subscription.snapshot;

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

		for(var i in me.list) {
			if(me.list[i].stage.id.applicationId.id == applicationId.id) {
				stages.push(me.list[i]);
			}
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
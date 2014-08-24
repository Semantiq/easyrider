app.service('Stages', ['Api', function(Api) {
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

	me.stagesOfApplication = function(applicationId) {
		var stages = [];

		for(var i in me.list) {
			if(me.list[i].stage.id.applicationId.id == applicationId.id) {
				stages.push(me.list[i]);
			}
		}

		return stages;
	};
}]);
// Create and initialize the experiment configuration object

// condition = 1;
// counterbalance = 1;

var $c = new Config(condition, counterbalance);

// Initalize psiturk object
var psiTurk = new PsiTurk(uniqueId, adServerLoc);

// Preload the HTML template pages that we need for the experiment
psiTurk.preloadPages($c.pages);

// Objects to keep track of the current phase and state
var CURRENTVIEW;
var STATE;

/*******************
 * Run Task
 ******************/

 $(document).ready(function() { 
	// Load the HTML for the trials
	psiTurk.showPage("trial.html");

	// Record various unstructured data
	psiTurk.recordUnstructuredData("condition", condition);
	psiTurk.recordUnstructuredData("counterbalance", counterbalance);

	// Start the experiment
	STATE = new State();

	// CURRENTVIEW = new Bridge();

	// Begin the experiment phase
	if (STATE.instructions) {
		// CURRENTVIEW = new Instructions();
		// CURRENTVIEW = new Demographics();
		// CURRENTVIEW = new Bridge();
		CURRENTVIEW = new TestPhase();
	}
});


/*************************
 * COUNTERBALANCING 
 *************************/
// determines whether ball A is on top or at the bottom of the screen 
// $c.condition = 0;
// $c.counterbalance = 0;

var position; 
if($c.counterbalance == 0){
	position = 'Atop';
}else{
	position = 'Abottom';
}
psiTurk.recordUnstructuredData('position',position);

/*************************
 * INSTRUCTIONS         
 *************************/

 var Instructions = function() {
	
 	this.trialinfo = $c.trials[$c.condition];
 	console.log("$c.trials[$c.condition]", $c.trials[$c.condition]);
	var instruction_slide;

	$(".slide").hide();
	var slide = $("#instructions"); //select instructions based on condition 
	slide.fadeIn($c.fade);

	psiTurk.recordUnstructuredData('structure',this.trialinfo.structure);
	psiTurk.recordUnstructuredData('pA',this.trialinfo.pA);
	psiTurk.recordUnstructuredData('pB',this.trialinfo.pB);
	
	var flip = "";
	if(position == 'Abottom'){
		flip = "_flipped";
	}

	$('.screenshot1').attr("src","static/images/initial_" + this.trialinfo.structure + "_" + (this.trialinfo.pA*100) + "_" + (this.trialinfo.pB*100) + flip+ ".png");
	
	if (this.trialinfo.pA == 0.2){
		$('.screenshot2').attr("src","static/images/A_blocked_" + this.trialinfo.structure + "_" + (this.trialinfo.pA*100) + "_" + (this.trialinfo.pB*100) + flip + ".png");	
	}else{
		$('.screenshot2').attr("src","static/images/B_blocked_" + this.trialinfo.structure + "_" + (this.trialinfo.pA*100) + "_" + (this.trialinfo.pB*100) + flip + ".png");
	}

	$('.screenshot3').attr("src","static/images/none_blocked_" + this.trialinfo.structure + "_" + (this.trialinfo.pA*100) + "_" + (this.trialinfo.pB*100) + flip + ".png");

	//figure 2 subcaption 
	if (this.trialinfo.pA == 0.2 && this.trialinfo.pB == 0.8){
		$('#fig2caption').html("Figure 2: <span style='color:#3EA333;font-weight:bold'>Ball A</span> was blocked. <span style='color:#2662E0;font-weight:bold'>Ball B</span> wasn't blocked. <b>Ball E</b> went through the gate.");
		$('#instruction_paragraph').html("Darker motion blocks are better at blocking balls than lighter ones. In the example below, there is a stronger motion block for <span style='color:#3EA333;font-weight:bold'>ball A</span> than there is for <span style='color:#2662E0;font-weight:bold'>ball B</span>. The brown solid block always blocks balls.");
	}else if (this.trialinfo.pA == 0.8 && this.trialinfo.pB == 0.2){
		$('#fig2caption').html("Figure 2: <span style='color:#3EA333;font-weight:bold'>Ball A</span> wasn't blocked. <span style='color:#2662E0;font-weight:bold'>Ball B</span> was blocked. <b>Ball E</b> went through the gate.");
		$('#instruction_paragraph').html("Darker motion blocks are better at blocking balls than lighter ones. In the example below, there is a stronger motion block for <span style='color:#2662E0;font-weight:bold'>ball B</span> than there is for <span style='color:#3EA333;font-weight:bold'>ball A</span>. The brown solid block always blocks balls.");
	}

	//structure paragraph 
	$('#structure').html("<span style='color:#3EA333;font-weight:bold'>Ball A</span> and <span style='color:#2662E0;font-weight:bold'>ball B</span> are set up in a way such that <b>ball E</b> only goes through the gate if either <span style='color:#3EA333;font-weight:bold'>Ball A</span> or <span style='color:#2662E0;font-weight:bold'>ball B</span> (but not both) hit <b>ball E</b>.")
	if (this.trialinfo.pA == 0.2){
		$('#fig2info').html("For example, in Figure 2, only <span style='color:#2662E0;font-weight:bold'>ball B</span> hit <b>ball E</b>, and <b>ball E</b> went through the gate.")
	}else{
		$('#fig2info').html("For example, in Figure 2, only <span style='color:#3EA333;font-weight:bold'>Ball A</span> hit <b>ball E</b>, and <b>ball E</b> went through the gate.")
	}
	
	slide.find('.next').click(function () {
		CURRENTVIEW = new Practice();
	});

};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*************************
 * PRACTICE
 *************************/
 var Practice = function(){
	$(".slide").hide();
	$("#practice").fadeIn($c.fade);
	
	//trialinfo determined based on condition 
	this.trialinfo = $c.trials[$c.condition];
	
	//Embed the physics world 
	html = '<iframe src="physics_world.html" width=800 height=600 id="game-frame"></iframe>';
	$('.physics_world_practice').html(html);

	var physics_world = document.getElementById('game-frame').contentWindow;
	$('#simulate').prop('disabled',false)

	var that = this;

	//read in and shuffle the learning trials 
	var learning_trials = shuffle(this.trialinfo.learning)
	console.log("learning_trials", learning_trials);
	
	update_progress(STATE.index, learning_trials.length);
	
	 //Add multiple choice questions 
	html = "<br/>" ;
	// Questions
	for (var i=0; i<$c.questions.practice.length; i++) {
		var q = $c.questions.practice[i][1];
		html += '<div id=' +  $c.questions.practice[i][0] +'>' + q + '   <input type = "radio" class = "radio" name = '+$c.questions.practice[i][0]+' value ="no">' + " no " + '<input type = "radio" class ="radio" name = '+$c.questions.practice[i][0]+' value ="yes">' + " yes " + '</div><br/><br/>' ;
	}
	$('#questions').html(html) ;

	//add event listeners to questions 
	for (var i=0; i<$c.questions.practice.length; i++) {
		$("input:radio[name='" + $c.questions.practice[i][0] + "']").change(function(){
				that.radioTest();
			});
	}

	//reveal css when all radio buttons have been pressed 
	this.radioTest = function() {      
		if($('#Ablock:not(:has(:radio:checked))').length == 0 && 
		   	$('#Bblock:not(:has(:radio:checked))').length == 0 &&
		   	$('#outcome:not(:has(:radio:checked))').length  == 0)
			{	
				$('#simulate').css('visibility','visible');
				$('#simulation_text').css('visibility','visible');
			} 
	};

	//record responses 
	this.responses = function(){
		var responses = [];
		for (var i=0; i<$c.questions.practice.length; i++) {
			responses[i] = $("input[name="+ $c.questions.practice[i][0] + "]:checked").val();
		}
		
		psiTurk.recordTrialData(['id', that.trialinfo.ID, 'blockA', learning_trials[STATE.index-1][0],'blockB', learning_trials[STATE.index-1][1], 'Ablock', responses[0], 'Bblock', responses[1], 'outcome', responses[2]]);		
		console.log(['id', that.trialinfo.ID, 'blockA', learning_trials[STATE.index-1][0],'blockB', learning_trials[STATE.index-1][1], 'Ablock', responses[0], 'Bblock', responses[1], 'outcome', responses[2]])
	}

	//start button 
	$('#simulate').click(function () {
		$('#simulate').prop('disabled',true)
		if (STATE.index != 0){
			that.responses();	
		}
		if (STATE.index < learning_trials.length){
			physics_world.Start(structure = that.trialinfo.structure, block1 = learning_trials[STATE.index][0], block2 = learning_trials[STATE.index][1],time = 300,pos = position, pA = that.trialinfo.pA, pB = that.trialinfo.pB);
			
			update_progress(STATE.index, learning_trials.length);

			//disable all radio buttons 
			$('.radio').prop('checked', false);
			$('#simulate').css('visibility','hidden');
			$('#simulation_text').css('visibility','hidden');
			STATE.set_index(STATE.index + 1);
		}else{
			CURRENTVIEW = new Bridge();
		}
	});
  }

/*************************™
 * BRIDGE         
 *************************/
 var Bridge = function() {
	$(".slide").hide();
	var slide = $("#bridge");
	slide.fadeIn($c.fade);

	slide.find('.next').click(function () {
		CURRENTVIEW = new TestPhase();
	});
};

/*************************
 * TRIAL
 *************************/
 var TestPhase = function(){
	$(".slide").hide();
	$("#test").fadeIn($c.fade);
	STATE.set_index(0) 
	this.trialinfo = $c.trials[$c.condition];
	var that = this;
	//Embed the physics world 
	html = '<iframe src="physics_world.html" width=800 height=600 id="game-frame2"></iframe>';
	$('#physics_world_test').html(html);

	var physics_world = document.getElementById('game-frame2').contentWindow;
	// $('#simulate2').prop('disabled',true)

	//load initial trajectory 
	physics_world.onload = function(){
		physics_world.Start(structure = that.trialinfo.structure, block1 = true, block2 = true, time = 30,pos = position, pA = that.trialinfo.pA, pB = that.trialinfo.pB);	
	}

        	$('#question_prompt').html("To what extent do you agree with the following statements?")
	//Add slider questions
	this.sliders = function(index){
		html = "";
		for (var i = 0; i < $c.questions[index].length; i++) {
			q = $c.questions[index][i][1];
			html += '<p class=".question" id = "'+ $c.questions[index][i][0] + '">' + q +'</p><div class="s-'+i+'"></div><div class="l-'+i+'"></div><br />' ;
		}
		$('#sliders').html(html);

		for (var i = 0; i < $c.questions[index].length; i++) {
			// Create the sliders
			$('.s-' + i).slider().on("slidestart", function(event, ui) {
			// Show the handle
			$(this).find('.ui-slider-handle').show();
			$('.ui-slider-handle').css('background','gray');

			// Sum is the number of sliders that have been clicked
			var sum = 0;
			for (var j = 0; j < $c.questions[index].length; j++) {
				if ($('.s-' + j).find('.ui-slider-handle').is(":visible")) {
				sum++;}
			}

			var limit; 
			if(index == 'prediction'){
				limit = $c.questions[index].length
			}else{
				limit = 1
			}

			// If the number of sliders clicked is equal to the number of sliders
			// the user can continue. 
			if (sum >= limit) {
				$('#simulate2').prop('disabled', false);
			}
		});
		// Put labels on the sliders
		$('.l-' + i).append("<label style='width: 33%'>not at all</label>");
		$('.l-' + i).append("<label style='width: 33%'></label>");
		$('.l-' + i).append("<label style='width: 33%'>very much</label>");
		}
	}
	this.sliders("prediction");
	
	// Hide all the slider handles 
	$('.ui-slider-handle').hide();

	//Flip question order based on condition and save which question came first 
	// psiTurk.recordUnstructuredData('order',this.trialinfo.order);	

	// Create the HTML for the radio buttons 
            var html = "" ; 
            for (var i=0; i<$c.questions.test.length; i++) {
                var q = $c.questions.test[i].q;
                html += '<input type = "radio" name = "sentence" value ='+$c.questions.test[i].type +' id = ' + $c.questions.test[i].type + '>' + q + '<br/><br/><br/>' ;
            }
            $('#choices').html(html) ;
            $('#choices').hide() ;

            for (var i=0; i<$c.questions.test.length; i++) {
                $('#' + $c.questions.test[i].type).change(function(){$('#simulate2').prop('disabled', false);
                });
            }

            // Disable button when one of the buttons is clicked
            $('#simulate2').prop('disabled', true);
	
	//record responses 
	this.responses = function(index){
		var response = [];		
		for (var i=0; i<$c.questions[index].length; i++) {
			response[i] =  $('.s-'+i).slider('value')   
			psiTurk.recordUnstructuredData($c.questions[index][i][0],response[i]);
		}
	}

	//start button 
	$('#simulate2').click(function () {
		switch(STATE.index){
			case 0:
				$('#question_prompt').html("Which of the following two statements better describes what happened?")
				that.responses("prediction");
				physics_world.Start(structure = that.trialinfo.structure, block1 = true, block2 = true, time = 300,pos = position, pA = that.trialinfo.pA, pB = that.trialinfo.pB);
				// that.sliders("test");
				// $('.ui-slider-handle').hide();
				$('#sliders').remove() ;
				$('#choices').show() ;
				$('#simulation_text2').html("<p>Please press 'Continue' to proceed.</p>");
				$('#simulate2').html("Continue");
			break;
			case 1: 
				// that.responses("test");
				var buttonPress = $('input[name = sentence]:checked').val()
				console.log("buttonPress", buttonPress);
        				psiTurk.recordUnstructuredData('sentenceChoice', buttonPress)
				CURRENTVIEW = new Demographics();
			break; 
		}
		STATE.set_index(STATE.index + 1);
		$('#simulate2').prop('disabled',true);
	});
  };

/*****************
 *  DEMOGRAPHICS*
 *****************/
 var Demographics = function(){

	var that = this; 

	// Show the slide
	$(".slide").hide();
	$("#demographics").fadeIn($c.fade);

		//disable button initially
		$('#trial_finish').prop('disabled', true);

		//checks whether all questions were answered
		$('.demoQ').change(function () {
		   if ($('input[name=sex]:checked').length > 0 &&
			 $('input[name=age]').val() != "")
		   {
			$('#trial_finish').prop('disabled', false)
		}else{
			$('#trial_finish').prop('disabled', true)
		}
	});

	// deletes additional values in the number fields 
	$('.numberQ').change(function (e) {    
		if($(e.target).val() > 100){
			$(e.target).val(100)
		}
	});

	this.finish = function() {
		debug("Finish test phase");

			// Show a page saying that the HIT is resubmitting, and
			// show the error page again if it times out or error
			var resubmit = function() {
				$(".slide").hide();
				$("#resubmit_slide").fadeIn($c.fade);

				var reprompt = setTimeout(prompt_resubmit, 10000);
				psiTurk.saveData({
					success: function() {
						clearInterval(reprompt); 
						finish();
					}, 
					error: prompt_resubmit
				});
			};

			// Prompt them to resubmit the HIT, because it failed the first time
			var prompt_resubmit = function() {
				$("#resubmit_slide").click(resubmit);
				$(".slide").hide();
				$("#submit_error_slide").fadeIn($c.fade);
			};

			// Render a page saying it's submitting
			psiTurk.showPage("submit.html") ;
			psiTurk.saveData({
				success: psiTurk.completeHIT, 
				error: prompt_resubmit
			});
		}; //this.finish function end 

		$('#trial_finish').click(function () {           
		   var feedback = $('textarea[name = feedback]').val();
		   var sex = $('input[name=sex]:checked').val();
		   var age = $('input[name=age]').val();

		   psiTurk.recordUnstructuredData('feedback',feedback);
		   psiTurk.recordUnstructuredData('sex',sex);
		   psiTurk.recordUnstructuredData('age',age);
		   that.finish();
	   });
};


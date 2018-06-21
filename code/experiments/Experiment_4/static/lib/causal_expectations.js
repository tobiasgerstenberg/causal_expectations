//box2d variables             
var b2Vec2 = Box2D.Common.Math.b2Vec2;
var b2BodyDef = Box2D.Dynamics.b2BodyDef;
var b2Body = Box2D.Dynamics.b2Body;
var b2FixtureDef = Box2D.Dynamics.b2FixtureDef;
var b2World = Box2D.Dynamics.b2World;
var b2PolygonShape = Box2D.Collision.Shapes.b2PolygonShape;
var b2WorldManifold = Box2D.Collision.b2WorldManifold;
var b2CircleShape = Box2D.Collision.Shapes.b2CircleShape;
var b2ContactListener = Box2D.Dynamics.b2ContactListener;

// Global variables
var world;
var bodies = [];  // instances of b2Body (from Box2D)
var actors = [];  // instances of Bitmap (from IvanK)
var scale = 100; //pixel to meter ratio; 1 meter = 100 pixels
var visualize = true;
var speed;
var ratio; //pixelratio (important for retina displays)
var stage; //the stage   
var fullX = 8
var fullY = 6
var ratio = window.devicePixelRatio;
var speed = 2;
// var speed = 0;
var collision = false; //records whether the balls collided 
var objectPositions = new Array();
var duration;
var outcome; //0 = ball missed, 1 = ball went through the gate
var topBlock;
var bottomBlock;
var position;
var pA;
var pB;

//START FUNCTION
function SetupWorld(){
	//setup world 
	var gravity = new b2Vec2(0,0) //neither gravity in x nor in y direction 
	world = new b2World(gravity);
	//counter for the world
	counter = 0;      

	if (visualize){
		if (stage === undefined) {
			stage = new Stage("c");
		}else{
		for (var i=0; i<actors.length; i++){
				  var actor=actors[i];
				  stage.removeChild(actor);
				}
				bodies = [];  // clear the body and actor vectors
				actors = []; 
		}
		endVis();
	}

	//symmetric overdetermination 
	objectPositions[0] = [
	fullX + 0.3, //Ball A 
	1.70, 
	-1, 
	.125, 
	fullX + 0.3, //Ball B 
	4.3, 
	-1, 
	-.125, 
	0.60, // Ball E 
	fullY/2, 
	0, 
	0, 
	0.5, // top Block 
	0.25,
	2,
	2.4,
	70, 
	0.5, // bottom block 
	0.25,
	2,
	3.6,
	290
	]

	//symmetric joint causation 
	objectPositions[1] = [
	fullX + 0.3, //Ball A 
	1.7,
	-1, 
	.22, 
	fullX + 0.3, //Ball B 
	4.3,
	-1, 
	-.22, 
	fullX/2-0.3, //Ball E 
	fullY/2, 
	0, 
	0,
	0.5, // top Block 
	0.25,
	4.5,
	2.4,
	70, 
	0.5, // bottom block 
	0.25,
	4.5,
	3.6,
	290]

	//xor
	objectPositions[2] = [
	fullX + 0.3, //Ball A 
	// fullX + 10, //Ball A 
	0.5,
	-1, 
	0.72, 
	fullX + 0.3, //Ball B 
	5.5,
	-1, 
	-0.72, 
	5, // Ball E 
	fullY/2, 
	0, 
	0, 
	0.5, // top Block 
	0.25,
	5.8,
	2.4,
	70, 
	0.5, // bottom block 
	0.25,
	5.8,
	3.6,
	290]

	//FUNCTION FOR CREATING WALLS 
	function createBox(w, h, x, y, type, userData, img){
	  // Create the fixture definition
	  var fixDef = new b2FixtureDef;

	  fixDef.density = 1; // Set the density
	  fixDef.friction = 0; // Set the friction 
	  fixDef.restitution = 0; // Set the restitution - elasticity

	  // Define the shape of the fixture
	  fixDef.shape = new b2PolygonShape;
	  fixDef.shape.SetAsBox(w , h); // input should be half the width and half the height
	  if(userData == 'topBlock' || userData == 'bottomBlock'){
		fixDef.isSensor = true 	
	  }

	  // Create the body definition
	  var bodyDef = new b2BodyDef;
	  bodyDef.type = type;

	  // Set the position of the body
	  bodyDef.position.x = x;
	  bodyDef.position.y = y;

	  // Create the body in the box2d world
	  var b = world.CreateBody(bodyDef);
	  b.CreateFixture(fixDef);

	  //assign user data (name)
	  b.SetUserData(userData);
	  
	  //add to list of bodies
	  bodies.push(b);

	  if (visualize){
		  //add image
		  var bd = new BitmapData(img);
		  var bm = new Bitmap(bd);  
		  bm.x = -width*scale; 
		  bm.y = -height*scale;
		  var actor = new Sprite(); 
		  actor.addChild(bm);        
		  actor.scaleX = ratio; 
		  actor.scaleY = ratio;
		  stage.addChild(actor);
		  actors.push(actor);
	  }

	  return b;
	}

	// FUNCTION FOR CREATING BALLS
	  function createBall(r, x, y, linx, liny, type, userData, img){
	  // Create the fixture definition
	  var fixDef = new b2FixtureDef;
	  fixDef.density = 1; // Set the density
	  fixDef.friction = 0; // Set the friction
	  fixDef.restitution = 1; // Set the restitution - bounciness

	  // Define the shape of the fixture
	  fixDef.shape = new b2CircleShape;
	  fixDef.shape.SetRadius(r);

	  // Create the body definition
	  var bodyDef = new b2BodyDef;
	  bodyDef.type = type;

	  // Set the position of the body
	  bodyDef.position.x = x;
	  bodyDef.position.y = y;

	  var linearVelocity  = new b2Vec2(linx,liny)

	  // Set the linear velocities 
	  bodyDef.linearVelocity = linearVelocity;
	  bodyDef.linearDamping = 0;
	  bodyDef.angularVelocity = 0;

	  // Create the body in the box2d world
	  var b = world.CreateBody(bodyDef);
	  b.CreateFixture(fixDef);

	  //assign user data (name)
	  b.SetUserData(userData);

	  //add to list of bodies 
	  bodies.push(b);

	 if (visualize){
		  //add image
		  var bd = new BitmapData(img);
		  var bm = new Bitmap(bd);  
		  bm.x = -50; 
		  bm.y = -50;
		  var actor = new Sprite(); 
		  actor.addChild(bm);        
		  actor.scaleX = ratio*r*2; 
		  actor.scaleY = ratio*r*2;
		  stage.addChild(actor);
		  actors.push(actor);
	  }

	  return b;
	}

	  
	   //SET UP THE WALLS 
	   //top wall 
	  createBox(
	  width = fullX/2, //width
	  height = 0.1, //height 
	  x = fullX/2,//x
	  y = 0.1, //y
	  b2Body.b2_staticBody, //what type of body
	  "topWall", //userdata 
	  "static/images/longWall.png") //image

	  //bottom wall 
	  createBox(
	  width = fullX/2, //width
	  height = 0.1, //height 
	  x = fullX/2,//x
	  y = fullY-0.1, //y
	  b2Body.b2_staticBody, //what type of body
	  "bottomWall", //userdata 
	  "static/images/longWall.png") //image
	  
	  //top-left wall 
	  createBox(
	  width = 0.1, //width
	  height = 1, //height 
	  x = 0.1,//x
	  y = 1, //y
	  b2Body.b2_staticBody, //what type of body
	  "topLeftWall", //userdata 
	  "static/images/shortWall.png") //image

	  //bottom-left wall 
	  createBox(
	  width = 0.1, //width
	  height = 1, //height 
	  x = 0.1,//x
	  y = 5, //y
	  b2Body.b2_staticBody, //what type of body
	  "topLeftWall", //userdata 
	  "static/images/shortWall.png") //image

	  // var clip = parent.document.getElementById("clip").value-1
	  // clip = 0

	  //hack to change position of block
	  var hack = [];
	  var hack2 = [];
	  if (position == "Abottom"){
	  	hack = [19,20,14,15];
	  	hack2 = [21,16];
	  }else{
		hack = [14,15,19,20];
		hack2 = [16,21];
	  }

	  //top block 
	  createBox(
	  width = objectPositions[clip][12], //width
	  height = objectPositions[clip][13], //height 	
	  x = objectPositions[clip][hack[0]],//x
	  y = objectPositions[clip][hack[1]], //y
	  b2Body.b2_staticBody, //what type of body
	  "topBlock", //userdata 
	  "static/images/block_red" + (Math.round((1-pA)*100)) + ".png") //image

	  //bottom block  
	  createBox(
	  width = objectPositions[clip][17], //width
	  height = objectPositions[clip][18], //height 
	  x = objectPositions[clip][hack[2]],//x
	  y = objectPositions[clip][hack[3]], //y
	  b2Body.b2_staticBody, //what type of body
	  "bottomBlock", //userdata 
	  "static/images/block_red" + (Math.round((1-pB)*100)) + ".png") //image

	  bodies[4].SetAngle(objectPositions[clip][hack2[0]]*Math.PI/180);
	  bodies[5].SetAngle(objectPositions[clip][hack2[1]]*Math.PI/180);

	  //brick 
	  createBox(
	  width = 0.5, //width
	  height = 0.25, //height 
	  x = 2.5,//x
	  y = 3, //y
	  b2Body.b2_staticBody, //what type of body
	  "brick", //userdata 
	  "static/images/block.png") //image
	  bodies[6].SetAngle(90*Math.PI/180);


	//COUNTERBALANCE THE POSITIONS OF THE BALLS
	if (position == 'Abottom'){
		objectPositions[clip][1] = 6-objectPositions[clip][1];
		objectPositions[clip][3] = objectPositions[clip][3]*(-1);
		objectPositions[clip][5] = 6-objectPositions[clip][1];
		objectPositions[clip][7] = objectPositions[clip][3]*(-1);
		// bodies[4].SetUserData("bottomBlock");
		// bodies[5].SetUserData("topBlock");
	}

	 // ADD THE BALLS 
	 createBall(
		0.33, //radius
		objectPositions[clip][0], //x position
		objectPositions[clip][1], //y position
		objectPositions[clip][2]*speed, //linear velocity in x direction
		objectPositions[clip][3]*speed, //linear velocity in y direction
		b2Body.b2_dynamicBody, //body type
		"ballA", //user data 
		"static/images/ballA.png" //image
		)

	createBall(
		0.33, //radius
		objectPositions[clip][4], //x position
		objectPositions[clip][5], //y position
		objectPositions[clip][6]*speed, //linear velocity in x direction
		objectPositions[clip][7]*speed, //linear velocity in y direction
		b2Body.b2_dynamicBody, //body type
		"ballB", //user data 
		"static/images/ballB.png" //image
		)

	createBall(
		0.33, //radius
		objectPositions[clip][8], //x position
		objectPositions[clip][9], //y position
		objectPositions[clip][10]*speed, //linear velocity in x direction
		objectPositions[clip][11]*speed, //linear velocity in y direction
		b2Body.b2_dynamicBody, //body type
		"ballE", //user data 
		"static/images/ballE.png" //image
		)

	//ADD CONTACT LISTENER
	var listener = new b2ContactListener();
	listener.BeginContact = function(contact) {
		a = contact.GetFixtureA().GetBody().GetUserData()
		b = contact.GetFixtureB().GetBody().GetUserData()
		//red block  
		if (topBlock){
			if((a == "ballA" & b == "topBlock") | (b == "ballA" & a == "topBlock")){
				bodies[7].SetAwake(false);
			}
		}
		//bottom block 
		if (bottomBlock){
			if((a == "ballB" & b == "bottomBlock") | (b == "ballB" & a == "bottomBlock")){
				bodies[8].SetAwake(false);
			}
		}
	}
	world.SetContactListener(listener);
	
	//RUN UPDATE FUNCTION 
	beginVis();
} // end Start()

//WORLD UPDATE FUNCTION
function animateWorld()
{
	world.Step(1/60,  5,  5);
	world.ClearForces();

	for(var i=0; i<actors.length; i++)
	{
		var body  = bodies[i];
		var actor = actors [i];
		var p = body.GetPosition();
		actor.x = p.x * ratio* scale;  
		actor.y = p.y *  ratio* scale;
		actor.rotation = body.GetAngle() * 180 / Math.PI;
	}

	counter++
	
	//end of clip
	if (counter == duration){
		for (var i = 0; i < bodies.length; i++) {
			var body = bodies[i];
			world.DestroyBody(body);
			$('#simulate', parent.document).prop('disabled',false);
			// var actor=actors[i];
			// stage.removeChild(actor);
		}
	}
}

//begin animation during visualization
function Start(structure,block1,block2,time,pos,pa, pb) {
	if(structure == 'disjunctive'){
		clip = 0 
	}else if(structure == 'conjunctive'){
		clip = 1
	}else if(structure == 'xor'){
		clip = 2 
	}
	position = pos // position of ball A (used for counterbalancing)
	duration = time;
	topBlock = block1;
	bottomBlock = block2;	
	pA = pa;
	console.log('pA', pA);
	pB = pb;
	console.log('pB', pB);
	visualize = true;
	SetupWorld();
}

//begin animation during visualization
function beginVis() {
	if (visualize) {stage.addEventListener(Event.ENTER_FRAME, animateWorld)}
}

//end animation during visualization
function endVis() {
	if (visualize) {stage.removeEventListener(Event.ENTER_FRAME, animateWorld)}
}

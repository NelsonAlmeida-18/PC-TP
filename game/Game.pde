import java.util.*;

enum Type {
  VERDE, // + v. angular
  AZUL, // + aceleração
  VERMELHO // volta ao 0
}

enum State {
  MENU,
  LOGIN,
  REGISTER,
  GAME,
  END
}

State state;
PImage img_game, img_menu;

// MENU
Button loginBtn;
Button registerBtn;
Button submitBtn;
Button backBtn;
InputField usernameField;
InputField passwordField;
String font;
int padding=20;

// GAME
float x, y;
float r_balls = 10;
float r_player = 35;
boolean alive = true;

Player player1;
ArrayList<Ball> balls = new ArrayList<Ball>();
int numBalls;
Timer gameTimer;
Timer genBallTimer;

void setup() {
  size(1280,720);
  setState(State.GAME);
  
  // MENU
  img_menu = loadImage("./Images/loginScreen.png");
  
  loginBtn = new Button("./Images/loginBtn.png", "./Images/loginBtnHover.png");
  loginBtn.updatePosition(width/2-loginBtn.width/2,height/2-loginBtn.height/2+padding);
  registerBtn = new Button("./Images/registerBtn.png", "./Images/registerBtnHover.png");
  registerBtn.updatePosition( loginBtn.x,loginBtn.y+loginBtn.height+padding);
 
  backBtn = new Button("./Images/closeScreen.png", "./Images/closeScreenHover.png");
  backBtn.updatePosition(width-(backBtn.width+padding), padding);
 
  usernameField = new InputField("./Images/inputBox.png", "Username...");
  usernameField.updatePosition(width/2-usernameField.width/2, height/2-usernameField.height/2+padding);
  passwordField = new InputField("./Images/inputBox.png", "Password...");
  passwordField.updatePosition( usernameField.x, usernameField.y+usernameField.height+padding);
 
  submitBtn = new Button("./Images/submitBtn.png", "./Images/submitBtnHover.png");
  submitBtn.updatePosition(passwordField.x+submitBtn.width/4,passwordField.y+passwordField.height+padding);
  
  // GAME
  img_game = loadImage("./Images/ringBG.png");
  
  float margin = r_player + 50; // 50 é uma margem de segurança
  do {
    x = random(margin, width - margin);
    y = random(margin, height - margin);
  } while (dist(x, y, width/2, height/2) < margin);
  player1 = new Player(x, y, r_player);
  
  gameTimer = new Timer(120);
  genBallTimer = new Timer(5);
}

void draw() {
  
  switch(state) {
    
    case MENU:
      surface.setSize(1280,720);
      image(img_menu,0,0);
      image(loginBtn.image,loginBtn.x, loginBtn.y);
      image(registerBtn.image,registerBtn.x, registerBtn.y);
      image(backBtn.image, backBtn.x, backBtn.y);
      
      balls = new ArrayList<Ball>();
      
      break;
      
    case LOGIN:
      surface.setSize(1280,720);
      image(img_menu, 0, 0);
      image(submitBtn.image, submitBtn.x, submitBtn.y);
      image(backBtn.image, backBtn.x, backBtn.y);
      image(usernameField.image, usernameField.x, usernameField.y);
      image(passwordField.image, passwordField.x, passwordField.y);
      textSize(20);
      fill(255);
      text(usernameField.text, usernameField.x + 10, usernameField.y + 25);
      text(passwordField.text, passwordField.x + padding, passwordField.y + passwordField.height / 2 + 73 / 2);
      
      break;
      
    case REGISTER:
      surface.setSize(1280,720);
      image(img_menu,0,0);
      image(submitBtn.image, submitBtn.x, submitBtn.y);
      image(backBtn.image, backBtn.x, backBtn.y);
      image(usernameField.image, usernameField.x, usernameField.y);
      image(passwordField.image, passwordField.x, passwordField.y);
      textSize(20);
      fill(255);
      text(usernameField.text, usernameField.x + 10, usernameField.y + 25);
      text(passwordField.text, passwordField.x + padding, passwordField.y + passwordField.height / 2 + 73 / 2);
      
      break;
    
    case GAME:
      surface.setSize(900, 900);
    
      image(img_game, 0, 0, width, height);

      player1.render();
      player1.update();
      player1.updateBonus();
          
      Iterator<Ball> ballIterator = balls.iterator();
      
      while (ballIterator.hasNext()) {
        Ball b = ballIterator.next();
        b.render();
        
        if (b.pos.dist(new PVector(player1.pos.x, player1.pos.y)) <= (r_balls + r_player)) {
            ballIterator.remove();
            player1.applyBonusEffect(b.type);
            numBalls--;
        }
        
      }
      
      Random rand = new Random();
      
      genBallTimer.countDown();
      if (genBallTimer.time <= 0) {
        if (numBalls < 5) {
          
          do {
            x = random(width);
            y = random(height);
          } while (x <= r_balls*3 || x >= width - r_balls*3 || y <= r_balls*3 || y >= height - r_balls*3);
          
          int randomNum = rand.nextInt(3);
          switch(randomNum) {
            case 0:
              balls.add(new Ball(x, y, r_balls, Type.VERDE));
              break;
            
            case 1:
              balls.add(new Ball(x, y, r_balls, Type.AZUL));
              break;
              
            case 2:
              balls.add(new Ball(x, y, r_balls, Type.VERMELHO));
              break;
          }
           
          numBalls++;
          genBallTimer.setTime(5);
          
        } else {
          
          genBallTimer.setTime(5);
          
        }
      }
      
      // Contador
      gameTimer.countDown();
      fill(0);
      textSize(24);
      text("Time left: " + int(gameTimer.getTime()) + " s", width/2 - 50, 30);
      
      // Quando o tempo acaba
      if (int(gameTimer.getTime()) == 0) {
        setState(State.END);
      }
      
      // Quando bate na borda
      if(player1.getX() <= r_player || player1.getX() >= width - r_player || player1.getY() <= r_player || player1.getY() >= height - r_player) {
        alive = false;
      }
      
      if (alive == false) {
        setState(State.END);
      }
    
      break;
    
    case END:
      surface.setSize(1280, 720);
      
      
      break;
  }

}

void setState(State newState) {
  state = newState;
}

void keyPressed() {
  player1.keyPressed();
  if (usernameField.isActive()){
    //atualiza campo com o input do utilizador
    usernameField.text=usernameField.value;
    usernameField.processKey(key);
  }
  
  if (passwordField.isActive()){
    //atualiza campo com o input do utilizador
    passwordField.text=passwordField.value;
    passwordField.processKey(key);
  }
}

void keyReleased() {
  player1.keyReleased();
}

void mouseClicked(){

  if(mouseX > loginBtn.x && mouseX<(loginBtn.x+loginBtn.width) && state == State.MENU){
    if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)){
      //passar para menu de login
      state = State.LOGIN;   
      loginBtn.reset();
    }
    //passar para menu de registo
    if(mouseY>registerBtn.y && mouseY<(registerBtn.y+registerBtn.height)){
      state = State.REGISTER;
      registerBtn.reset();
    }
  }

 
  if(mouseX > usernameField.x && mouseX<(usernameField.x+usernameField.width) && (state == State.LOGIN || state == State.REGISTER)){
    if(mouseY>usernameField.y && mouseY<(usernameField.y+usernameField.height)){
      //atualiza o username
      usernameField.activate();
      passwordField.deactivate();

    }
    if(mouseY>passwordField.y && mouseY<(passwordField.y+passwordField.height)){
      //Atualiza a password
      passwordField.activate();
      usernameField.deactivate();
    }
  }
  else{
    if(state == State.LOGIN){
      passwordField.deactivate();
      usernameField.deactivate();
    }
  }
  
  if(mouseX>backBtn.x && mouseX<backBtn.x+backBtn.width && mouseY>backBtn.y && mouseY<backBtn.y+backBtn.height){
    if (state == State.MENU){
       exit();
    }
    else{
      state = State.MENU;
      usernameField.reset();
      passwordField.reset();
      backBtn.reset();
     
    }
  }
}

void mouseMoved(){
  if (state == State.MENU){
    //atualiza o botão para hover
    if(mouseX > loginBtn.x && mouseX<(loginBtn.x+loginBtn.width)){
      if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)){
        loginBtn.image=loginBtn.hover;
        registerBtn.image=registerBtn.regular;
      }
      if(mouseY>registerBtn.y && mouseY<(registerBtn.y+registerBtn.height)){
          registerBtn.image=registerBtn.hover;
          loginBtn.image=loginBtn.regular;
      }
    }
    else{
       loginBtn.image=loginBtn.regular;
       registerBtn.image=registerBtn.regular;
       backBtn.image=backBtn.regular;
    }
  }
  
  if(mouseX>backBtn.x && mouseX<backBtn.x+backBtn.width && mouseY>backBtn.y && mouseY<backBtn.y+backBtn.height){
    backBtn.image=backBtn.hover;
  }
  else{
    if(mouseX>submitBtn.x && mouseX<submitBtn.x+submitBtn.width){
        if(mouseY>submitBtn.y && mouseY<submitBtn.y+submitBtn.height){
           submitBtn.image=submitBtn.hover; 
        }
    }
    else{
       submitBtn.image=submitBtn.regular; 
    }
  }
}

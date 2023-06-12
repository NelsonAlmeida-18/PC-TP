import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

enum State {
    MENU,
    LOGIN,
    REGISTER,
    GAME,
    PLAY,
    END,
    findingMatch
}

enum Type {
    VERDE, // + v. angular
    AZUL, // + aceleração
    VERMELHO // volta ao 0
}

State state;
ConnectionManager cm;
PImage img_menu, img_game;

// MENU
Button loginBtn;
Button registerBtn;
Button submitBtn;
Button backBtn;
Button playBtn;
Button playAgainBtn;
Button exitBtn;
InputField usernameField;
InputField passwordField;
String font;
int padding=20;
Boolean findingMatch=false;
Boolean readerCreated=false;
Boolean startedBalls=false;
Boolean userLoggedIn=false;
Boolean waitingInLobby=false;


String  matchStatus="";


// GAME
Player player1 = new Player(255,0,0);
Player player2 = new Player(0,255,0);

ArrayList<Ball> balls = new ArrayList<Ball>();

String time;

void setup() {
    size(1280,720);
    setState(State.MENU);
    try{
        cm = new ConnectionManager("192.168.0.101", 9000);
        //verificar se o cm se conseguiu conectar
        // Notificar o cliente que não dá para entrar 
      //dar await enquanto o connectionManager não se conseguir conectar
      //print("Não se conseguiu conectar ao servidor");
   
    }
    catch(Exception e){
    }
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

    playBtn = new Button("./Images/play.png", "./Images/playHover.png");
    playBtn.updatePosition(width/2-playBtn.width/2, loginBtn.y);


    submitBtn = new Button("./Images/submitBtn.png", "./Images/submitBtnHover.png");
    submitBtn.updatePosition(passwordField.x+submitBtn.width/4,passwordField.y+passwordField.height+padding);
  
    playAgainBtn = new Button("./Images/playAgainBtn.png", "./Images/playAgainHover.png");
    playAgainBtn.updatePosition(loginBtn.x, loginBtn.y);

    exitBtn = new Button("./Images/exitBtn.png", "./Images/exitBtnHover.png");
    exitBtn.updatePosition(registerBtn.x, registerBtn.y);
    
    // GAME
    img_game = loadImage("./Images/ringBG.png");
}

void draw() {
    switch(state) {
        case MENU:
            surface.setSize(1280,720);
            image(img_menu,0,0);
            image(loginBtn.image,loginBtn.x, loginBtn.y);
            image(registerBtn.image,registerBtn.x, registerBtn.y);
            image(backBtn.image, backBtn.x, backBtn.y);

            break;
            
        case PLAY:
          image(img_menu,0,0);
          if(userLoggedIn){
             fill(0,255,0);
             textSize(24);
             text("User logged in",10,30);
          }
          else if(waitingInLobby){
            fill(0,255,0);
             textSize(24);
             text("User joined the lobby",10,60);
          }
          image(backBtn.image, backBtn.x, backBtn.y);
          image(playBtn.image, playBtn.x, playBtn.y);
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
            player2.render();
            
            fill(this.player1.r, this.player1.g, this.player1.b);
            textSize(24);
            text(this.player1.name+": "+ str(this.player1.p), 10,30);
            fill(this.player2.r, this.player2.g, this.player2.b);
            textSize(24);
            String str = this.player2.name+": "+str(this.player2.p);
            text(str, width-textWidth(str)-10, 30);

            // parser("P,"+ProcessHandle.current().pid()+",150,100,11,321,350,500,0,30");
            // parser("B,0,100,500,1,200,350");
            if (readerCreated==false){
               readerCreated=true;
               new Thread( () -> {
                 try{              
                   String debug = this.cm.receive();
                   while(debug!=null){
                     parser(debug);
                     debug=this.cm.receive();
                   }
                 }
                 catch(Exception e){
                   e.printStackTrace();}
               }).start();
            }
        
            Iterator<Ball> ballIterator = balls.iterator();

            while (ballIterator.hasNext()) {
                Ball b = ballIterator.next();
                b.render();
            }

            // Contador
            fill(0);
            textSize(24);
            text("Time left: " + time + " s",width/2 - 50, 30);

            break;

        case END:
            surface.setSize(1280, 720);
            
            if (matchStatus.equals("yw"))
                  image(loadImage("./Images/YouWon_BG.jpg"),0,0);
            else if(matchStatus.equals("yl"))
                image(loadImage("./Images/YouLost_BG.jpg"),0,0);
            else if(matchStatus.equals("tie"))
                  image(loadImage("./Images/Tie.png"),0,0);
            image(playAgainBtn.image, playAgainBtn.x, playAgainBtn.y);
            image(exitBtn.image, exitBtn.x, exitBtn.y);
            break;
    }
}

// "P,123,100,100,0,321,300,300,0,30"
// "B,0,100,500,1,200,350"
void parser(String string) {
    if (string.charAt(0)=='P') { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,timeLeft]
        String[] players = string.split(";");
        for(String str : players){
        String[] tokens = str.split(",");
          if (tokens[1].equals(this.usernameField.value)) { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,timeLeft]
              player1.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Float.parseFloat(tokens[4]), Integer.parseInt(tokens[5]), tokens[1]);
              //player2.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
          } else {
              //player1.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
              player2.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Float.parseFloat(tokens[4]), Integer.parseInt(tokens[5]), tokens[1]);
          }
        }
    }


    else if (string.charAt(0) == 'I') { 
      balls = new ArrayList<Ball>();
      String[] parsed = string.split(";");
      for (String str : parsed){
        String[] tokens = str.split(",");
        float x = Float.parseFloat(tokens[2]);
        float y = Float.parseFloat(tokens[3]);
        switch(tokens[1]) {
            case "green":
                balls.add(new Ball(x, y, Type.VERDE));
                break;
  
            case "red":
                balls.add(new Ball(x, y, Type.AZUL));
                break;
  
            case "blue":
                balls.add(new Ball(x, y, Type.VERMELHO));
                break;
        }
      }
    }
    else if(string.charAt(0)=='T'){
       String[] temp = string.split(",");
       Float tempTime = Float.parseFloat(temp[1])/1000;
       time = str(int(tempTime));
    }
    
    else if(string.equals("You won!")){
      this.state=State.END;
       this.matchStatus="yw"; 
    }
   else if(string.equals("You lost!")){
     this.state=State.END;
       this.matchStatus="yl"; 
    }
    else if(string.equals("Tie!")){
      this.state=State.END;
       this.matchStatus="tie";
    }

}

void setState(State newState) {
    state = newState;
}

void keyPressed() {
    

    if (usernameField.isActive()) {
        //atualiza campo com o input do utilizador
        usernameField.text=usernameField.value;
        usernameField.processKey(key);
    }
    if (passwordField.isActive()) {
        //atualiza campo com o input do utilizador
        passwordField.text=passwordField.value;
        passwordField.processKey(key);
    }
    if(state==State.GAME){
       this.cm.sendKey(key); 
    }
}

void keyReleased() {
  if(state==State.GAME)
    this.cm.releaseKey(key);
}

void mouseClicked(){
  if(mouseX > loginBtn.x && mouseX<(loginBtn.x+loginBtn.width) && state==State.MENU){
    if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)){
      //passar para menu de login
      state=State.LOGIN;   
      loginBtn.reset();
    }
    //passar para menu de registo
    if(mouseY>registerBtn.y && mouseY<(registerBtn.y+registerBtn.height)){
      state=State.REGISTER;
      registerBtn.reset();
    }
  }

 
  if(mouseX > usernameField.x && mouseX<(usernameField.x+usernameField.width) && (state==State.LOGIN || state==State.REGISTER)){
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
    if(state==State.LOGIN){
      passwordField.deactivate();
      usernameField.deactivate();
    }
  }
  
  //verificar se o botão de submit foi clicado, se sim, fazer request e analisar a resposta
  //caso esteja no login e este seja válido transita para o ecrã de play
  //caso esteja no registo e este seja válido transita para o ecrã main
  if(mouseX>submitBtn.x && mouseX<submitBtn.x+submitBtn.width && mouseY>submitBtn.y && mouseY<submitBtn.y+submitBtn.height){
     //verificar se está no ecrã de login
     if(state==State.LOGIN){
        //fazer request e analisar a resposta  
        this.cm.loginUser(usernameField.value, passwordField.value);
        String response = this.cm.receive();
        if(response.equals("Logged in with success!")){
              state = State.PLAY;
              userLoggedIn=true;
              submitBtn.reset();
        }
     }
     if(state==State.REGISTER){
        this.cm.registerUser(usernameField.value, passwordField.value);
        String response = this.cm.receive();
        if(response.equals("User created!")){
              state = State.MENU;
              usernameField.reset();
              passwordField.reset();
              submitBtn.reset();
        }
     }
  }
  
  //verificar se o botão de play foi clicado, se sim, ficar à espera de encontrar partida e fazer animação DIY
  if(state==State.PLAY){
      if(mouseX>playBtn.x && mouseX<playBtn.x+playBtn.width && mouseY>playBtn.y && mouseY<playBtn.y+playBtn.height){
        
        if(!waitingInLobby){
          this.cm.joinMatch(usernameField.value);
          String response = this.cm.receive();
         
          if (response.equals("User joined the lobby!")){
             waitingInLobby=true; 
             fill(0,255,0);
             textSize(24);
             text("User joined the lobby",10,60);
          }
          response = this.cm.receive();
          if(response.equals("Start")){
            state = State.GAME;
            userLoggedIn=false;
          }
        }
     } 
  }
  
  if (state == State.END) {
    if(mouseX > playAgainBtn.x && mouseX<(playAgainBtn.x + playAgainBtn.width)) {
        if(mouseY>playAgainBtn.y && mouseY<(playAgainBtn.y + playAgainBtn.height)) {
          if(!waitingInLobby){
            this.cm.joinMatch(usernameField.value);
            String response = this.cm.receive();
           
            if (response.equals("User joined the lobby!")){
               waitingInLobby=true; 
               fill(0,255,0);
               textSize(24);
               text("User joined the lobby",10,60);
            }
            response = this.cm.receive();
            if(response.equals("Start")){
              state = State.GAME;
              userLoggedIn=false;
          }
        }
        if(mouseY>exitBtn.y && mouseY<(exitBtn.y + exitBtn.height)) {
            exitBtn.image = exitBtn.hover;
            playAgainBtn.image = playAgainBtn.regular;
            cursor(HAND);
            
        }
      }
    }
  }
  if(mouseX>backBtn.x && mouseX<backBtn.x+backBtn.width && mouseY>backBtn.y && mouseY<backBtn.y+backBtn.height){
    if (state==State.MENU){
       exit();
    }
    else{
      state=State.MENU;
      usernameField.reset();
      passwordField.reset();
      backBtn.reset();
      playBtn.reset();
    }
  }
}


void mouseMoved() {
    if (state == State.MENU) {
        //atualiza o botão para hover
        if(mouseX > loginBtn.x && mouseX<(loginBtn.x+loginBtn.width)) {
            if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)) {
                loginBtn.image=loginBtn.hover;
                registerBtn.image=registerBtn.regular;
                cursor(HAND);
            }
            if(mouseY>registerBtn.y && mouseY<(registerBtn.y+registerBtn.height)) {
                registerBtn.image=registerBtn.hover;
                loginBtn.image=loginBtn.regular;
                cursor(HAND);
            }
        } else {
            loginBtn.image=loginBtn.regular;
            registerBtn.image=registerBtn.regular;
            backBtn.image=backBtn.regular;
            cursor(ARROW);
        }
    }

    if(mouseX>backBtn.x && mouseX<backBtn.x+backBtn.width && mouseY>backBtn.y && mouseY<backBtn.y+backBtn.height) {
        backBtn.image=backBtn.hover;
        cursor(HAND);
    } else {
        if(mouseX>submitBtn.x && mouseX<submitBtn.x+submitBtn.width) {
            if(mouseY>submitBtn.y && mouseY<submitBtn.y+submitBtn.height) {
                submitBtn.image=submitBtn.hover;
                cursor(HAND);
            }
        } else {
            submitBtn.image=submitBtn.regular;
            cursor(ARROW);
        }
    }

    if (state == State.END) {
        if(mouseX > playAgainBtn.x && mouseX<(playAgainBtn.x + playAgainBtn.width)) {
            if(mouseY>playAgainBtn.y && mouseY<(playAgainBtn.y + playAgainBtn.height)) {
                playAgainBtn.image = playAgainBtn.hover;
                exitBtn.image = exitBtn.regular;
                cursor(HAND);
            }
            if(mouseY>exitBtn.y && mouseY<(exitBtn.y + exitBtn.height)) {
                exitBtn.image = exitBtn.hover;
                playAgainBtn.image = playAgainBtn.regular;
                cursor(HAND);
                
            }
        }
    }
}

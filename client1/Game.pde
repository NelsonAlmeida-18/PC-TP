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
    findingMatch,
}

enum Type {
    VERDE, // + v. angular
    AZUL, // + aceleração
    VERMELHO // volta ao 0
}

State state;
PImage img_menu, img_game;
PImage won_BG, lost_BG;

// MENU
Button loginBtn;
Button registerBtn;
Button submitBtn;
Button backBtn;
Button playBtn;
InputField usernameField;
InputField passwordField;
Button playAgainBtn;
Button exitBtn;
String font;
int padding=20;
Boolean findingMatch=false;
ConnectionManager cm;
boolean won = false;
String success_message = "";
String error_message = "";


// GAME
Player player1 = new Player();
Player player2 = new Player();

ArrayList<Ball> balls = new ArrayList<Ball>();

String time;

void setup() {
    size(1280,720);
    setState(State.MENU);

    try{
      cm = new ConnectionManager("172.30.196.149", 9000);
      //verificar se o cm se conseguiu conectar
      //Notificar o cliente que não dá para entrar
      //dar await enquanto o connectionManager não se conseguir conectar
      //print("Não se conseguiu conectar ao servidor");
      player1.addCM(cm);
      player2.addCM(cm);
    } catch(Exception e) {
      e.printStackTrace();
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
    playAgainBtn.updatePosition(width/2-playAgainBtn.width/2,height/2-playAgainBtn.height/2+padding);

    exitBtn = new Button("./Images/exitBtn.png", "./Images/exitBtnHover.png");
    exitBtn.updatePosition(playAgainBtn.x, playAgainBtn.y + playAgainBtn.height+padding);

    // GAME
    img_game = loadImage("./Images/ringBG.png");

    // END
    won_BG = loadImage("./Images/YouWon_BG.jpg");
    lost_BG = loadImage("./Images/YouLost_BG.jpg");
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

            // parser("P,"+ProcessHandle.current().pid()+",150,100,11,321,350,500,0,30");
            // parser("B,0,100,500,1,200,350");

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

            if (won) {
                image(won_BG, 0, 0);
            } else {
                image(lost_BG, 0, 0);
            }

            image(playAgainBtn.image, playAgainBtn.x, playAgainBtn.y);
            image(exitBtn.image, exitBtn.x, exitBtn.y);

            break;
    }
}

// "P,123,100,100,0,321,300,300,0,30"
// "B,0,100,500,1,200,350"
void parser(String string) {
    String[] tokens = string.split(",");

    if (tokens[0].equals("P")) { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,timeLeft]
        if(Integer.parseInt(tokens[1]) == ProcessHandle.current().pid()) {
            player1.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Integer.parseInt(tokens[4]));
            player2.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));

        } else {
            player1.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
            player2.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Integer.parseInt(tokens[4]));
        }

        this.time = tokens[9];

    } else if (tokens[0].equals("B")) { // [B,t1,x1,y1,t2,x2,y2,t3,x3,y3,t4,x4,y4,t5,x5,y5]
        int size = tokens.length;
        balls = new ArrayList<Ball>();
        // 1, 4, 7, 10, 13, 16
        int i = 1;
        while (i < size) {
            int t = Integer.parseInt(tokens[i]);
            float x = Float.parseFloat(tokens[i + 1]);
            float y = Float.parseFloat(tokens[i + 2]);

            switch(t) {
                case 0:
                    balls.add(new Ball(x, y, Type.VERDE));
                    break;

                case 1:
                    balls.add(new Ball(x, y, Type.AZUL));
                    break;

                case 2:
                    balls.add(new Ball(x, y, Type.VERMELHO));
                    break;
            }

            i += 3;
        }

    } else if (tokens[0].equals("You won!")) {
        this.won = true;

    } else if (tokens[0].equals("You lost!")) {
        this.won = false;

    } else if (tokens[0].equals("User already exists!")) {
        error_message = "User already exists!";

    } else if (tokens[0].equals("User created!")) {
        success_message = "User created!";

    } else if (tokens[0].equals("User does not exist!")) {
        error_message = "User does not exist!";

    } else if (tokens[0].equals("Invalid password!")) {
        error_message = "Invalid password!";

    } else if (tokens[0].equals("Account deleted with success!")) {
        success_message = "Account deleted with success!";

    } else if (tokens[0].equals("Logged in with success!")) {
        success_message = "Logged in with success!";

    } else if (tokens[0].equals("Logged out with success!")) {
        success_message = "Logged out with success!";

    } else if (tokens[0].equals("User joined the lobby!")) {
        success_message = "User joined the lobby!";

    } else if (tokens[0].equals("User not logged in!")) {
        error_message = "User not logged in!";
    }
}

void setState(State newState) {
    state = newState;
}

void keyPressed() {
    player1.keyPressed();
    player2.keyPressed();

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
}

void keyReleased() {
    player1.keyReleased();
    player2.keyReleased();
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

        this.cm.joinMatch(usernameField.value);
        String response = this.cm.receive();
        print(response);
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

  if(mouseX>exitBtn.x && mouseX<exitBtn.x + exitBtn.width && mouseY>exitBtn.y && mouseY<exitBtn.y + exitBtn.height) {
    if (state == State.END) {
      exit();
    }
  }

  if(mouseX>playAgainBtn.x && mouseX<playAgainBtn.x + playAgainBtn.width && mouseY>playAgainBtn.y && mouseY<playAgainBtn.y + playAgainBtn.height) {
    if (state == State.END) {
      state = State.MENU;
      playAgainBtn.reset();
    }
  }
}

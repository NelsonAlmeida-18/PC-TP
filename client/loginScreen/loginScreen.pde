PImage bg;
Button loginBtn;
Button registerBtn;
Button submitBtn;
Button backBtn;
Window window=new Window();
InputField usernameField;
InputField passwordField;
String font;
int padding=20;

enum Screen{
  main,
  login,
  register
}

Screen screen;

void setup(){
 size(1280,720);
 bg = loadImage("./src/loginScreen.png");
 
 loginBtn = new Button("./src/loginBtn.png", "./src/loginBtnHover.png");
 loginBtn.updatePosition( window.width/2-loginBtn.width/2,window.height/2-loginBtn.height/2+padding);
 registerBtn = new Button("./src/registerBtn.png", "./src/registerBtnHover.png");
 registerBtn.updatePosition( loginBtn.x,loginBtn.y+loginBtn.height+padding);
 
 backBtn = new Button("./src/closeScreen.png", "./src/closeScreenHover.png");
 backBtn.updatePosition(window.width-(backBtn.width+padding), padding);
 
 usernameField = new InputField("./src/inputBox.png", "Username...");
 usernameField.updatePosition( window.width/2-usernameField.width/2,window.height/2-usernameField.height/2+padding);
 passwordField = new InputField("./src/inputBox.png", "Password...");
 passwordField.updatePosition( usernameField.x, usernameField.y+usernameField.height+padding);
 
 submitBtn = new Button("./src/submitBtn.png", "./src/submitBtnHover.png");
 submitBtn.updatePosition(passwordField.x+submitBtn.width/4,passwordField.y+passwordField.height+padding);
 screen=Screen.main;
}

void draw(){
  if (screen==Screen.main){
     image(bg,0,0);
     image(loginBtn.image,loginBtn.x, loginBtn.y);
     image(registerBtn.image,registerBtn.x, registerBtn.y);
     image(backBtn.image, backBtn.x, backBtn.y);
  }
  else{
    image(bg, 0, 0);
    image(submitBtn.image, submitBtn.x, submitBtn.y);
    image(backBtn.image, backBtn.x, backBtn.y);
    image(usernameField.image, usernameField.x, usernameField.y);
    image(passwordField.image, passwordField.x, passwordField.y);
    textSize(20);
    fill(255);
    text(usernameField.text, usernameField.x + 10, usernameField.y + 25);
    text(passwordField.text, passwordField.x + padding, passwordField.y + passwordField.height / 2 + 73 / 2); 
  }
}

void mouseClicked(){

  if(mouseX > loginBtn.x && mouseX<(loginBtn.x+loginBtn.width) && screen==Screen.main){
    if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)){
      //passar para menu de login
      screen=Screen.login;   
      loginBtn.reset();
    }
    //passar para menu de registo
    if(mouseY>registerBtn.y && mouseY<(registerBtn.y+registerBtn.height)){
      screen=Screen.register;
      registerBtn.reset();
    }
  }

 
  if(mouseX > usernameField.x && mouseX<(usernameField.x+usernameField.width) && (screen==Screen.login || screen==Screen.register)){
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
    if(screen==Screen.login){
      passwordField.deactivate();
      usernameField.deactivate();
    }
  }
  
  if(mouseX>backBtn.x && mouseX<backBtn.x+backBtn.width && mouseY>backBtn.y && mouseY<backBtn.y+backBtn.height){
    if (screen==Screen.main){
       exit();
    }
    else{
      screen=Screen.main;
      usernameField.reset();
      passwordField.reset();
      backBtn.reset();
     
    }
  }
}

void mouseMoved(){
  if (screen==Screen.main){
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

void keyPressed(){
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


////////////////////////////////////////////////////////////////////////////////
class Window{
  int width=1280;
  int height=720;
  
  Window(){
  };
  
   Window(int width, int height){
    this.width=width;
    this.height=height;
  }
}

class Button{
 PImage image;
 PImage hover;
 PImage regular;
 float x;
 float y;
 float width;
 float height;
 
 Button(String path){
   this.image=loadImage(path);
   this.regular=loadImage(path);
   this.hover=loadImage(path);
   this.width=this.image.width;
   this.height=this.image.height;
 }
 
  Button(String path, String hoverPath){
   this.image=loadImage(path);
   this.hover = loadImage(hoverPath);
   this.regular = this.image;
   this.width=this.image.width;
   this.height=this.image.height;
 }
 
 void updatePosition(float x, float y){
   this.x=x;
   this.y=y;
 }
 
 void reset(){
  this.image=this.regular; 
 }
}

class InputField{
 PImage image;
 float x;
 float y;
 float width;
 float height;
 String text="";
 //default text to store
 String dft="";
 String value="";
 boolean isActive;
 
 InputField(String path){
   this.image=loadImage(path);
   this.width=this.image.width;
   this.height=this.image.height;
 }
 
 InputField(String path, String text){
   this.image=loadImage(path);
   this.width=this.image.width;
   this.height=this.image.height;
   this.text=text;
   this.dft=text;
 }
 
 void updateText(String text){
   this.text=text;
 }
 
 void updatePosition(float x, float y){
   this.x=x;
   this.y=y;
 }
 
 void activate(){
    this.isActive=true; 
 }
 
 void deactivate(){
    this.isActive=false; 
 }
 
  boolean isActive(){
    return this.isActive;
  }
 
 void processKey(char key){
   if(this.isActive ){
     if (key=='')
       deleteChar();
     else
       this.value+=key;
   }
 }
 
 void deleteChar(){
      if (value.length() > 0){        
            value = value.substring(0,value.length()-1);
      }
  }
  
  void reset(){
        this.value = "";
        this.text=this.dft;
 }
  
}

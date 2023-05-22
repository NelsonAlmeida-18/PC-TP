import java.util.*;

enum Type {
  VERDE, // + v. angular
  AZUL, // + aceleração
  VERMELHO // volta ao 0
}

enum State {
  MENU,
  GAME,
  END
}

State state = State.GAME;
PImage img;
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
  size(900, 900);
  
  img = loadImage("./Images/background_grass.jpg");
  
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
      // setState(State.MENU);
      surface.setSize(1200, 900);
      
      balls = new ArrayList<Ball>();
      
      
      break;
    
    case GAME:
      // setState(State.GAME);
      surface.setSize(900, 900);
    
      image(img, 0, 0, width, height);

      player1.render();
      player1.update();
      player1.updateBonus();
      
      fill(0);
      textSize(24);
      text(player1.pos.x, 50, 50);
    
      fill(0);
      textSize(24);
      text(player1.pos.y, 50, 100);
          
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
      surface.setSize(1200, 900);
      
      
      break;
  }

}

void setState(State newState) {
  state = newState;
}

void keyPressed() {
  player1.keyPressed();
}

void keyReleased() {
  player1.keyReleased();
}

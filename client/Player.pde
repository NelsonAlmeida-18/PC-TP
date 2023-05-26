class Player {
  PVector pos;
  float r;
  float speed = 0;
  float s_base = 3.5;
  float angle = 0;
  float a_base = 0.1;
  boolean accelerating = false;
  
  int verdeBonusTimeLeft = 0;
  int azulBonusTimeLeft = 0;
  
  Player(float x, float y, float r_) {
    this.pos = new PVector(x,y);
    this.r = r_;
  }
  
  void update() {
    if (accelerating) {
      speed = s_base;
    } else {
      speed = 0;
    }
  
    speed = constrain(speed, 0, 7);
    
    pos.x += speed * cos(angle);
    pos.y += speed * sin(angle);
  }
  
  void render() {
    pushMatrix();
    translate(pos.x, pos.y);
    rotate(angle);
    stroke(255);
    fill(255, 0, 0);
    ellipse(0, 0, r*2, r*2);
    line(0, 0, r, 0);
    popMatrix();
  }
  
  void keyPressed() {
    if (key == 'w') {
      accelerating = true;
    } else if (key == 'd') {
      angle += a_base;
    } else if (key == 'a') {
      angle -= a_base;
    }
  }

  void keyReleased() {
    if (key == 'w') {
      accelerating = false;
    }
  }

  float getX() {
    return pos.x;
  }

  float getY() {
    return pos.y;
  }

  void applyBonusEffect(Type type) {
    switch(type) {
      case VERDE:
        a_base *= 1.5;
        verdeBonusTimeLeft = 300; // 5 segundos
        break;
    
      case AZUL:
        s_base *= 1.5;
        azulBonusTimeLeft = 300; // 5 segundos
        break;
        
      case VERMELHO:
        a_base = 0.1;
        s_base = 3.5;
        break;
    }
  }
  
  void updateBonus() {
    if (verdeBonusTimeLeft > 0) {
      verdeBonusTimeLeft--;
      if (verdeBonusTimeLeft == 0) {
        a_base /= 1.5;
      }
    }
    if (azulBonusTimeLeft > 0) {
      azulBonusTimeLeft--;
      if (azulBonusTimeLeft == 0) {
        s_base /= 1.5;
      }
    }
  }
  
}

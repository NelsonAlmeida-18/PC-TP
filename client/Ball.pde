class Ball {
  PVector pos;
  float r;
  Type type;
  
  Ball(float x, float y, float r_, Type t) {
    this.pos = new PVector(x,y);
    this.r = r_;
    this.type = t;
  }
  
  void render() {
    switch(type) {
      case VERDE:
        fill(color(109,190,69));
        break;
      
      case AZUL:
        fill(color(1,149,255));
        break;
        
      case VERMELHO:
        fill(color(252,33,34));
        break;
    }
    
    circle(pos.x, pos.y, r*2);
  }
}

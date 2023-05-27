class Ball {
    PVector pos;
    float r = 10;
    Type type;

    Ball(float x, float y, Type t) {
        this.pos = new PVector(x,y);
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

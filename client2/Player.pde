class Player {
    PVector pos = new PVector(100,100); // (x,y)
    float rad; // raio
    int p; // pontuação
    float angle;
    float r;
    float g;
    float b;
    String name = "";

    Player(float r, float g, float b) {
        this.rad = 35;
        this.p = 0;
        this.r = r;
        this.g=g;
        this.b=b;
    }

    void updatePlayer(float x, float y, float angle, int p, String name) {
        this.pos.x = x;
        this.pos.y = y;
        this.p = p;
        this.angle = angle;
        this.name=name;
    }

    void render() {
        pushMatrix();
        translate(pos.x, pos.y);
        rotate(angle);
        stroke(255);
        fill(this.r, this.g, this.b);
        ellipse(0, 0, rad*2, rad*2);
        line(0, 0, rad, 0);
        popMatrix();
    }

}

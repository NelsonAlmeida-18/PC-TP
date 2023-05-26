class Player {
    long pid;
    PVector pos = new PVector(100, 100); // (x,y)
    float r; // raio
    int p; // pontuação

    Player() {
        this.r = 35;
        this.p = 0;
    }

    Player(long pid) {
        this.pid = pid;
        this.r = 35;
        this.p = 0;
    }

    void updatePlayer(float x, float y, int p) {
        this.pos.set(x,y);
        this.p = p;
    }

    void render() {
        pushMatrix();
        translate(pos.x, pos.y);
        // rotate(angle);
        stroke(255);
        fill(255, 0, 0);
        ellipse(0, 0, r*2, r*2);
        line(0, 0, r, 0);
        popMatrix();
    }

    void keyPressed() {
        if (key == 'w') {

        } else if (key == 'd') {

        } else if (key == 'a') {

        }
    }

    void keyReleased() {
        if (key == 'w') {

        }
    }

    float getX() {
        return pos.x;
    }

    float getY() {
        return pos.y;
    }
}

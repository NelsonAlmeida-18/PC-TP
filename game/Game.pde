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
    END
}

enum Type {
    VERDE, // + v. angular
    AZUL, // + aceleração
    VERMELHO // volta ao 0
}

PImage img_game;
State state;

Player player1;
Player player2;

ArrayList<Ball> balls = new ArrayList<Ball>();

String time;

void setup() {
    size(1280,720);
    setState(State.GAME);
    img_game = loadImage("./Images/ringBG.png");
}

void draw() {
    switch(state) {
        case GAME:
            surface.setSize(900, 900);
            image(img_game, 0, 0, width, height);

            player1 = new Player(ProcessHandle.current().pid());
            player1.render();

            player2 = new Player();
            player2.render();

            Iterator<Ball> ballIterator = balls.iterator();

            while (ballIterator.hasNext()) {
                Ball b = ballIterator.next();
                b.render();
            }

            // Contador
            fill(0);
            textSize(24);
            text("Time left: " + time + " s", width/2 - 50, 30);

            break;
    }
}

void parser(String string) {
    String[] tokens = string.split(",");

    if (tokens[0].equals("P")) { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,timeLeft]
        if(Integer.parseInt(tokens[1]) == ProcessHandle.current().pid()) {
            player1.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Integer.parseInt(tokens[4]));
        }
        else {
            player2.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
        }
        this.time=tokens[9];

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

            i += 3
        }
    }
}

void setState(State newState) {
    state = newState;
}

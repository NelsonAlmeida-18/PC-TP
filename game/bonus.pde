//Tipos de Bonus
enum BonusType{
    GREEN,
    BLUE,
    RED
}

class BonusObject { 
    float x, y;
    BonusType objectType;
    
    // Construtor que recebe as coordenadas e o tipo do objeto
    BonusObject(float x, float y, BonusType objectType) { 
        this.x = x; 
        this.y = y; 
        this.objectType = objectType; 
    }
    
    void display() { // Desenha o objeto bonus
        pushMatrix();
        translate(x, y);
        if (objectType == BonusType.GREEN) {
            fill(0, 255, 0); // Objeto verde
            
        } else if (objectType == BonusType.BLUE) {
            fill(0, 0, 255); // Objeto azul
            
        } else {
            fill(255, 0, 0); // Objeto vermelho
        }
        ellipse(0, 0, 20, 20);
        popMatrix();
    }
}

void createBonusObject() {
    //novo objeto de bonus é criado com uma probabilidade de 1% e < 15 ao msm tempo
    if (random(1) < 0.01 && bonusObjects.size() < 15) { 
        
        //25 serve para que os objetos não sejam criados muito junto das bordas
        float objectX = random(25, width - 25); 
        float objectY = random(25, height - 25);
        // Escolha aleatoria
        BonusType objectType = BonusType.values()[(int)random(3)];     
        // Adiciona à lista
        bonusObjects.add(new BonusObject(objectX, objectY, objectType)); 
    }
}

// Função que reduz gradualmente a velocidade linear e angular com o tempo
void reduceSpeeds() { 
    float reductionFactorLinear = 0.9999;
    float reductionFactorAngle = 0.999;
    linearSpeedBase = max(linearSpeedBase * reductionFactorLinear, 2);
    angularSpeedBase = max(angularSpeedBase * reductionFactorAngle, PI / 10);
}


void checkCollision() {
    
    // Iterar os objetos da lista
    for (int i = bonusObjects.size() - 1; i >= 0; i--) {
        BonusObject obj = bonusObjects.get(i);
        
        // desenhar o objeto bonus na tela
        obj.display();
        
        // calcula a distancia entre o jogador e o objeto
        float distance = dist(player.x, player.y, obj.x, obj.y);
        
        //verifica a distancia entre eles se é < 35
        if (distance < 35) {
            
            //aplica o efeito
            applyBonusEffect(obj);
            
            //remove da lista
            removeCollectedObject(i);
        }
    }
}

//Retira o objeto bonus da lista 
void removeCollectedObject(int index) {
    
    bonusObjects.remove(index);
}


// Função que aplica o efeito do bonus
void applyBonusEffect(BonusObject obj) {
    
    if (obj.objectType == BonusType.GREEN) {
        angularSpeedBase = min(angularSpeedBase * 1.5, maxAngle);
        
    } else if (obj.objectType == BonusType.BLUE) {
        linearSpeedBase = min(linearSpeedBase * 1.5, maxSpeed);
        
    } else if (obj.objectType == BonusType.RED) {
        angularSpeedBase = PI / 10;
        linearSpeedBase = 2;
    }
}
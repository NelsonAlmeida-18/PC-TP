//Valores máximo do bonus que um player pode obter
float maxSpeed = 10; 
float maxAngle = (PI / 10) * 5;

//Valores iniciais do player
float linearSpeedBase = 2; 
float angularSpeedBase = PI / 10;

//Background Img
PImage img;

// Objeto que representa o jogador
Player player = new Player(400, 300); 

//Lista que armazena todos os objetos bonus do mundo
ArrayList<BonusObject> bonusObjects = new ArrayList<BonusObject>(); 

//Duração de uma partida
int duration = 90 * 1000; 

//Tempo em q a partida começou
int startTime; 

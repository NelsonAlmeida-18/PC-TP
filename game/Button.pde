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
   width=this.image.width;
   height=this.image.height;
 }
 
  Button(String path, String hoverPath){
   this.image=loadImage(path);
   this.hover = loadImage(hoverPath);
   this.regular = this.image;
   width=this.image.width;
   height=this.image.height;
 }
 
 void updatePosition(float x, float y){
   this.x=x;
   this.y=y;
 }
 
 void reset(){
  this.image=this.regular; 
 }
}

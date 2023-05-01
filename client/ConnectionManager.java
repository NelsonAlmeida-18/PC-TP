package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ConnectionManager implements AutoCloseable {

    Socket s;
    BufferedReader in;
    PrintWriter out;

    public ConnectionManager(Socket socket) throws IOException{
        this.s = socket;
        this.in = new BufferedReader(new InputStreamReader(this.s.getInputStream()));
        this.out = new PrintWriter(s.getOutputStream());
    }
    
    public static ConnectionManager start(Socket socket) throws IOException{
        return new ConnectionManager(socket);
    }
    
    public void send(String type, String message) throws IOException{
        out.println(type + ":" + message);
        out.flush();
    }

    public String receive(String type) throws IOException{
        String res = this.in.readLine();
        return res;
    }

    public void close() throws IOException{
        this.s.close();
        this.in.close();
        this.out.close();
    }
}

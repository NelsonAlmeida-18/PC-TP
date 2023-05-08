package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.locks.*;

public class ConnectionManager implements AutoCloseable {

    private Socket s;
    private BufferedReader in;
    private PrintWriter out;
    private Condition notEmpty;
    private int readers = 0, writers = 0;
    private Lock l;

    public ConnectionManager(Socket socket) throws IOException{
        this.s = socket;
        this.in = new BufferedReader(new InputStreamReader(this.s.getInputStream()));
        this.out = new PrintWriter(s.getOutputStream());
        this.l = new ReentrantLock();
    }
    
    public static ConnectionManager start(Socket socket) throws IOException{
        return new ConnectionManager(socket);
    }
    
    public void send(String type, String message) throws IOException{
        this.l.lock();
        try{
            this.out.println(type + ":" + message);
            this.out.flush();
        }finally{
            l.unlock();
        }
    }

    public String receive(String type) throws IOException{
        this.l.lock();
        String res;
        try{
            res = this.in.readLine();
        }finally{
            this.l.unlock();
        }

        return res;
    }

    public void close() throws IOException{
        this.l.lock();
        try{
            this.s.close();
            this.in.close();
            this.out.close();
        }finally{
            this.l.unlock();
        }
    }
}

package haven;

public class LoginData {
    public String name;
    public String pass;

    public LoginData(String name, String pass) {
        this.name = name;
        this.pass = pass;
    }

    @Override
    public boolean equals(Object other){
        if (other == null) return false;
        if (other == this) return true;
        if (!(other instanceof LoginData))return false;
        LoginData ol = (LoginData)other;
        return ol.name.equals(name) && ol.pass.equals(pass);
    }
}
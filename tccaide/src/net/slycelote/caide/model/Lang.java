package net.slycelote.caide.model;

public enum Lang {
    CPP("cpp", "cpp"), CSHARP("cs", "c#"), UNSUPPORTED("", "");
    public final String extension;
    public final String name;

    private Lang(String extension, String name) {
        this.extension = extension;
        this.name = name;
    }

}

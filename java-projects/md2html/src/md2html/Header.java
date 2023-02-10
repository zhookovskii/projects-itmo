package md2html;

class Header {
    private StringBuilder input;

    Header(StringBuilder input) {
        this.input = input;
    }

    private int level(StringBuilder s) {
        int i = 0;
        while (i < s.length() && s.charAt(i) == '#') {
            i++;
        }
        return i;
    }

    public void toHtml(StringBuilder current) {
        int level = level(input);
        current.append("<h").append(level).append(">");
        StringBuilder following = new StringBuilder(input.substring(level + 1));
        new Text(following).toHtml(current);
        current.append("</h").append(level).append(">");
    }
}

package haven.purus.pathfinder;

public class Line {

	public double constant, slope;

	// Line is vertical if slope is infinite and then constant = x
	public Line(double constant, double slope) {
		this.constant = constant;
		this.slope = slope;
	}

	public boolean isVertical() {
		if(slope == Double.POSITIVE_INFINITY)
			return true;
		else
			return false;
	}

	public boolean isHorizontal() {
		if(slope == 0)
			return true;
		else
			return false;
	}

	public double xAtY(int y) {
		if(slope == Double.POSITIVE_INFINITY)
			return slope; // Vertical
		else
			return (y-constant)/slope;
	}

	public String toString() {
		return "(" + constant + ", " + slope + ")";
	}
}

interface Mensch {
    double eiergroesse();
    double auslaenderhaftigkeit();
    double haarausfall();
}

class Henry implements Mensch {

	@Override
	public double eiergroesse() {
		return 1000;
	}

	@Override
	public double auslaenderhaftigkeit() {
		return 100_000;
	}

	@Override
	public double haarausfall() {
		return 0;
	}
}

class Kenan implements Mensch {
	@Override
	public double eiergroesse() {
		return 10;
	}

	@Override
	public double auslaenderhaftigkeit() {
		return 1_000_000;
	}

	@Override
	public double haarausfall() {
		return 1;
	}
}

class Devin implements Mensch {

	@Override
	public double eiergroesse() {
		return 50;
	}

	@Override
	public double auslaenderhaftigkeit() {
		return 10000;
	}

	@Override
	public double haarausfall() {
		return 0.2;
	}

}

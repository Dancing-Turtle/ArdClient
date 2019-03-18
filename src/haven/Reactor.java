package haven;

import rx.subjects.PublishSubject;

public class Reactor {
    /** Publishes all 'error' messages from server*/
    public static final PublishSubject<String> EMSG = PublishSubject.create();

    /** Publishes all 'info' messages from server*/
    public static final PublishSubject<String> IMSG = PublishSubject.create();

    /** Publishes changes to player name */
    public static final PublishSubject<String> PLAYER = PublishSubject.create();
}

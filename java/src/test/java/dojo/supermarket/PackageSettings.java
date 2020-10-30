package dojo.supermarket;

import org.approvaltests.core.ApprovalFailureReporter;
import org.approvaltests.reporters.JunitReporter;

public class PackageSettings {
    public static ApprovalFailureReporter UseReporter         = JunitReporter.INSTANCE;
}
